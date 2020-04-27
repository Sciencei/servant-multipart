{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
-- | @multipart/form-data@ support for servant.
--
--   This is mostly useful for adding file upload support to
--   an API. See haddocks of 'MultipartForm' for an introduction.
module Servant.Multipart
  ( MultipartForm
  , MultipartData(..)
  , FromMultipart(..)
  , lookupInput
  , lookupFile
  , MultipartOptions(..)
  , defaultMultipartOptions
  , MultipartBackend(..)
  , TmpBackendOptions(..)
  , defaultTmpBackendOptions
  , Input(..)
  , FileData(..)
  -- * servant-client
  , genBoundary
  , ToMultipart(..)
  , multipartToBody
  -- * servant-docs
  , ToMultipartSample(..)
  ) where

import Control.Lens ((<>~), (&), view, (.~))
import Control.Monad (replicateM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Array (listArray, (!))
import Data.Foldable (foldMap, foldl')
import Data.List (find)
import Data.Maybe
import Data.Monoid
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable
import Network.HTTP.Media.MediaType ((//), (/:))
import Network.Wai
import Network.Wai.Parse
import Servant
import Servant.Client.Core (HasClient(..), RequestBody(RequestBodySource), setRequestBody)
import Servant.Docs
import Servant.Foreign
import Servant.Server.Internal
import Servant.Types.SourceT (SourceT(..), source, StepT(..), fromActionStep)
import System.Directory
import System.IO (IOMode(ReadMode), withFile)
import System.Random (getStdRandom, Random(randomR))

import Servant.Multipart.Types

import qualified Data.ByteString      as SBS
import qualified Data.ByteString.Lazy as LBS

fromRaw :: forall tag. ([Network.Wai.Parse.Param], [File (MultipartResult tag)])
        -> MultipartData tag
fromRaw (inputs, files) = MultipartData is fs

  where is = map (\(name, val) -> Input (dec name) (dec val)) inputs
        fs = map toFile files

        toFile :: File (MultipartResult tag) -> FileData tag
        toFile (iname, fileinfo) =
          FileData (dec iname)
                   (dec $ fileName fileinfo)
                   (dec $ fileContentType fileinfo)
                   (fileContent fileinfo)

        dec = decodeUtf8

-- | Lookup a file input with the given @name@ attribute.
lookupFile :: Text -> MultipartData tag -> Maybe (FileData tag)
lookupFile iname = find ((==iname) . fdInputName) . files

-- | Lookup a textual input with the given @name@ attribute.
lookupInput :: Text -> MultipartData tag -> Maybe Text
lookupInput iname = fmap iValue . find ((==iname) . iName) . inputs

-- | Upon seeing @MultipartForm a :> ...@ in an API type,
---  servant-server will hand a value of type @a@ to your handler
--   assuming the request body's content type is
--   @multipart/form-data@ and the call to 'fromMultipart' succeeds.
instance ( FromMultipart tag a
         , MultipartBackend tag
         , LookupContext config (MultipartOptions tag)
         , HasServer sublayout config )
      => HasServer (MultipartForm tag a :> sublayout) config where

  type ServerT (MultipartForm tag a :> sublayout) m =
    a -> ServerT sublayout m

#if MIN_VERSION_servant_server(0,12,0)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy sublayout) pc nt . s
#endif

  route Proxy config subserver =
    route psub config subserver'
    where
      psub  = Proxy :: Proxy sublayout
      pbak  = Proxy :: Proxy b
      popts = Proxy :: Proxy (MultipartOptions tag)
      multipartOpts = fromMaybe (defaultMultipartOptions pbak)
                    $ lookupContext popts config
      subserver' = addMultipartHandling pbak multipartOpts subserver

-- | Upon seeing @MultipartForm a :> ...@ in an API type,
--   servant-client will take a parameter of type @(LBS.ByteString, a)@,
--   where the bytestring is the boundary to use (see 'genBoundary'), and
--   replace the request body with the contents of the form.
instance (ToMultipart tag a, HasClient m api, MultipartBackend tag)
      => HasClient m (MultipartForm tag a :> api) where

  type Client m (MultipartForm tag a :> api) = 
    (LBS.ByteString, a) -> Client m api

  clientWithRoute pm _ req (boundary, param) =
      clientWithRoute pm (Proxy @api) $ setRequestBody newBody newMedia req
    where
      newBody = multipartToBody boundary $ toMultipart @tag param
      newMedia = "multipart" // "form-data" /: ("boundary", LBS.toStrict boundary)

  hoistClientMonad pm _ f cl = \a ->
      hoistClientMonad pm (Proxy @api) f (cl a)

-- | Generates a boundary to be used to separate parts of the multipart.
-- Requires 'IO' because it is randomized.
genBoundary :: IO LBS.ByteString
genBoundary = LBS.pack
            . map (validChars !)
            <$> indices
  where
    -- the standard allows up to 70 chars, but most implementations seem to be
    -- in the range of 40-60, so we pick 55
    indices = replicateM 55 . getStdRandom $ randomR (0,61)
    -- Following Chromium on this one:
    -- > The RFC 2046 spec says the alphanumeric characters plus the
    -- > following characters are legal for boundaries:  '()+_,-./:=?
    -- > However the following characters, though legal, cause some sites
    -- > to fail: (),./:=+
    -- https://github.com/chromium/chromium/blob/6efa1184771ace08f3e2162b0255c93526d1750d/net/base/mime_util.cc#L662-L670
    validChars = listArray (0 :: Int, 61)
                           -- 0-9
                           [ 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37
                           , 0x38, 0x39, 0x41, 0x42
                           -- A-Z, a-z
                           , 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a
                           , 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52
                           , 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a
                           , 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68
                           , 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70
                           , 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78
                           , 0x79, 0x7a
                           ]


-- Try and extract the request body as multipart/form-data,
-- returning the data as well as the resourcet InternalState
-- that allows us to properly clean up the temporary files
-- later on.
check :: MultipartBackend tag
      => Proxy tag
      -> MultipartOptions tag
      -> DelayedIO (MultipartData tag)
check pTag tag = withRequest $ \request -> do
  st <- liftResourceT getInternalState
  rawData <- liftIO
      $ parseRequestBodyEx
          parseOpts
          (backend pTag (backendOptions tag) st)
          request
  return (fromRaw rawData)
  where parseOpts = generalOptions tag

-- Add multipart extraction support to a Delayed.
addMultipartHandling :: forall tag multipart env a. (FromMultipart tag multipart, MultipartBackend tag)
                     => Proxy tag
                     -> MultipartOptions tag
                     -> Delayed env (multipart -> a)
                     -> Delayed env a
addMultipartHandling pTag opts subserver =
  addBodyCheck subserver contentCheck bodyCheck
  where
    contentCheck = withRequest $ \request ->
      fuzzyMultipartCTCheck (contentTypeH request)

    bodyCheck () = do
      mpd <- check pTag opts :: DelayedIO (MultipartData tag)
      case fromMultipart mpd of
        Nothing -> liftRouteResult $ FailFatal
          err400 { errBody = "fromMultipart returned Nothing" }
        Just x -> return x

    contentTypeH req = fromMaybe "application/octet-stream" $
          lookup "Content-Type" (requestHeaders req)

-- Check that the content type is one of:
--   - application/x-www-form-urlencoded
--   - multipart/form-data; boundary=something
fuzzyMultipartCTCheck :: SBS.ByteString -> DelayedIO ()
fuzzyMultipartCTCheck ct
  | ctMatches = return ()
  | otherwise = delayedFailFatal err400 {
      errBody = "The content type of the request body is not in application/x-www-form-urlencoded or multipart/form-data"
      }

  where (ctype, attrs) = parseContentType ct
        ctMatches = case ctype of
          "application/x-www-form-urlencoded" -> True
          "multipart/form-data" | Just _bound <- lookup "boundary" attrs -> True
          _ -> False

-- | Global options for configuring how the
--   server should handle multipart data.
--
--   'generalOptions' lets you specify mostly multipart parsing
--   related options, such as the maximum file size, while
--   'backendOptions' lets you configure aspects specific to the chosen
--   backend. Note: there isn't anything to tweak in a memory
--   backend ('Mem'). Maximum file size etc. options are in
--   'ParseRequestBodyOptions'.
--
--   See haddocks for 'ParseRequestBodyOptions' and
--   'TmpBackendOptions' respectively for more information on
--   what you can tweak.
data MultipartOptions tag = MultipartOptions
  { generalOptions        :: ParseRequestBodyOptions
  , backendOptions        :: MultipartBackendOptions tag
  }


-- | Configuration for the temporary file based backend.
--
--   You can configure the way servant-multipart gets its hands
--   on a temporary directory (defaults to 'getTemporaryDirectory')
--   as well as the filename pattern used for generating the temporary files
--   (defaults to calling them /servant-multipartXXX.buf/, where /XXX/ is some
--   random number).
data TmpBackendOptions = TmpBackendOptions
  { getTmpDir   :: IO FilePath
  , filenamePat :: String
  }

-- | Default options for the temporary file backend:
--   'getTemporaryDirectory' and "servant-multipart.buf"
defaultTmpBackendOptions :: TmpBackendOptions
defaultTmpBackendOptions = TmpBackendOptions
  { getTmpDir = getTemporaryDirectory
  , filenamePat = "servant-multipart.buf"
  }

-- | Default configuration for multipart handling.
--
--   Uses 'defaultParseRequestBodyOptions' and
--   'defaultBackendOptions' respectively.
defaultMultipartOptions :: MultipartBackend tag => Proxy tag -> MultipartOptions tag
defaultMultipartOptions pTag = MultipartOptions
  { generalOptions = defaultParseRequestBodyOptions
  , backendOptions = defaultBackendOptions pTag
  }

-- Utility class that's like HasContextEntry
-- but allows the lookup to fail, to make a context
-- entry for upload config optional (hence using
-- some default configuration when missing)
class LookupContext ctx a where
  lookupContext :: Proxy a -> Context ctx -> Maybe a

instance LookupContext '[] a where
  lookupContext _ _ = Nothing

instance {-# OVERLAPPABLE #-}
         LookupContext cs a => LookupContext (c ': cs) a where
  lookupContext p (c :. cs) =
    lookupContext p cs

instance {-# OVERLAPPING #-}
         LookupContext cs a => LookupContext (a ': cs) a where
  lookupContext _ (c :. _) = Just c

instance HasLink sub => HasLink (MultipartForm tag a :> sub) where
#if MIN_VERSION_servant(0,14,0)
  type MkLink (MultipartForm tag a :> sub) r = MkLink sub r
  toLink toA _ = toLink toA (Proxy :: Proxy sub)
#else
  type MkLink (MultipartForm tag a :> sub) = MkLink sub
  toLink _ = toLink (Proxy :: Proxy sub)
#endif

-- | The 'ToMultipartSample' class allows you to create sample 'MultipartData'
-- inputs for your type for use with "Servant.Docs".  This is used by the
-- 'HasDocs' instance for 'MultipartForm'.
--
-- Given the example 'User' type and 'FromMultipart' instance above, here is a
-- corresponding 'ToMultipartSample' instance:
--
-- @
--   data User = User { username :: Text, pic :: FilePath }
--
--   instance 'ToMultipartSample' 'Tmp' User where
--     'toMultipartSamples' proxy =
--       [ ( \"sample 1\"
--         , 'MultipartData'
--             [ 'Input' \"username\" \"Elvis Presley\" ]
--             [ 'FileData'
--                 \"pic\"
--                 \"playing_guitar.jpeg\"
--                 \"image/jpeg\"
--                 \"/tmp/servant-multipart000.buf\"
--             ]
--         )
--       ]
-- @
class ToMultipartSample tag a where
  toMultipartSamples :: Proxy a -> [(Text, MultipartData tag)]

-- | Format an 'Input' into a markdown list item.
multipartInputToItem :: Input -> Text
multipartInputToItem (Input name val) =
  "        - *" <> name <> "*: " <> "`" <> val <> "`"

-- | Format a 'FileData' into a markdown list item.
multipartFileToItem :: FileData tag -> Text
multipartFileToItem (FileData name _ contentType _) =
  "        - *" <> name <> "*, content-type: " <> "`" <> contentType <> "`"

-- | Format a description and a sample 'MultipartData' into a markdown list
-- item.
multipartSampleToDesc
  :: Text -- ^ The description for the sample.
  -> MultipartData tag -- ^ The sample 'MultipartData'.
  -> Text -- ^ A markdown list item.
multipartSampleToDesc desc (MultipartData inputs files) =
  "- " <> desc <> "\n" <>
  "    - textual inputs (any `<input>` type but file):\n" <>
  foldMap (\input -> multipartInputToItem input <> "\n") inputs <>
  "    - file inputs (any HTML input that looks like `<input type=\"file\" name=\"somefile\" />`):\n" <>
  foldMap (\file -> multipartFileToItem file <> "\n") files

-- | Format a list of samples generated with 'ToMultipartSample' into sections
-- of markdown.
toMultipartDescriptions
  :: forall tag a.
     ToMultipartSample tag a
  => Proxy tag -> Proxy a -> [Text]
toMultipartDescriptions _ proxyA = fmap (uncurry multipartSampleToDesc) samples
  where
    samples :: [(Text, MultipartData tag)]
    samples = toMultipartSamples proxyA

-- | Create a 'DocNote' that represents samples for this multipart input.
toMultipartNotes
  :: ToMultipartSample tag a
  => Int -> Proxy tag -> Proxy a -> DocNote
toMultipartNotes maxSamples' proxyTag proxyA =
  let sampleLines = take maxSamples' $ toMultipartDescriptions proxyTag proxyA
      body =
        [ "This endpoint takes `multipart/form-data` requests.  The following is " <>
          "a list of sample requests:"
        , foldMap (<> "\n") sampleLines
        ]
  in DocNote "Multipart Request Samples" $ fmap unpack body

-- | Declare an instance of 'ToMultipartSample' for your 'MultipartForm' type
-- to be able to use this 'HasDocs' instance.
instance (HasDocs api, ToMultipartSample tag a) => HasDocs (MultipartForm tag a :> api) where
  docsFor
    :: Proxy (MultipartForm tag a :> api)
    -> (Endpoint, Action)
    -> DocOptions
    -> API
  docsFor _ (endpoint, action) opts =
    let newAction =
          action
            & notes <>~
                [ toMultipartNotes
                    (view maxSamples opts)
                    (Proxy :: Proxy tag)
                    (Proxy :: Proxy a)
                ]
    in docsFor (Proxy :: Proxy api) (endpoint, newAction) opts

instance (HasForeignType lang ftype a, HasForeign lang ftype api)
      => HasForeign lang ftype (MultipartForm t a :> api) where
  type Foreign ftype (MultipartForm t a :> api) = Foreign ftype api

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy @api) $
      req & reqBody .~ Just t
          & reqBodyContentType .~ ReqBodyMultipart
    where
      t = typeFor lang ftype (Proxy @a)

instance MultipartBackend Tmp where
    type MultipartResult Tmp = FilePath
    type MultipartBackendOptions Tmp = TmpBackendOptions

    defaultBackendOptions _ = defaultTmpBackendOptions
    -- streams the file from disk 
    loadFile _ fp =
        SourceT $ \k ->
        withFile fp ReadMode $ \hdl ->
        k (readHandle hdl)
      where
        readHandle hdl = fromActionStep LBS.null (LBS.hGet hdl 4096)
    backend _ opts = tmpBackend
      where
        tmpBackend = tempFileBackEndOpts (getTmpDir opts) (filenamePat opts)

instance MultipartBackend Mem where
    type MultipartResult Mem = LBS.ByteString
    type MultipartBackendOptions Mem = ()

    defaultBackendOptions _ = ()
    loadFile _ = source . pure
    backend _ opts _ = lbsBackEnd

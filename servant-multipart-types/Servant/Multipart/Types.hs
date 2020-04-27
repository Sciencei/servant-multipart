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
module Servant.Multipart.Types
  ( MultipartForm
  , MultipartData(..)
  , FromMultipart(..)
  , MultipartBackend(..)
  , Tmp
  , Mem
  , Input(..)
  , FileData(..)
  -- * servant-client
  , ToMultipart(..)
  , multipartToBody
  ) where

import Control.Monad.Trans.Resource
import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Servant.Client.Core (RequestBody(RequestBodySource))
import Servant.Foreign
import Servant.Types.SourceT (SourceT(..), source, StepT(..), fromActionStep)
import Data.Proxy

import qualified Data.ByteString      as SBS
import qualified Data.ByteString.Lazy as LBS

-- | Combinator for specifying a @multipart/form-data@ request
--   body, typically (but not always) issued from an HTML @\<form\>@.
--
--   @multipart/form-data@ can't be made into an ordinary content
--   type for now in servant because it doesn't just decode the
--   request body from some format but also performs IO in the case
--   of writing the uploaded files to disk, e.g in @/tmp@, which is
--   not compatible with servant's vision of a content type as things
--   stand now. This also means that 'MultipartForm' can't be used in
--   conjunction with 'ReqBody' in an endpoint.
--
--   The 'tag' type parameter instructs the function to handle data
--   either as data to be saved to temporary storage ('Tmp') or saved to
--   memory ('Mem').
--
--   The 'a' type parameter represents the Haskell type to which
--   you are going to decode the multipart data to, where the
--   multipart data consists in all the usual form inputs along
--   with the files sent along through @\<input type="file"\>@
--   fields in the form.
--
--   One option provided out of the box by this library is to decode
--   to 'MultipartData'.
--
--   Example:
--
--   @
--   type API = MultipartForm Tmp (MultipartData Tmp) :> Post '[PlainText] String
--
--   api :: Proxy API
--   api = Proxy
--
--   server :: MultipartData Tmp -> Handler String
--   server multipartData = return str
--
--     where str = "The form was submitted with "
--              ++ show nInputs ++ " textual inputs and "
--              ++ show nFiles  ++ " files."
--           nInputs = length (inputs multipartData)
--           nFiles  = length (files multipartData)
--   @
--
--   You can alternatively provide a 'FromMultipart' instance
--   for some type of yours, allowing you to regroup data
--   into a structured form and potentially selecting
--   a subset of the entire form data that was submitted.
--
--   Example, where we only look extract one input, /username/,
--   and one file, where the corresponding input field's /name/
--   attribute was set to /pic/:
--
--   @
--   data User = User { username :: Text, pic :: FilePath }
--
--   instance FromMultipart Tmp User where
--     fromMultipart multipartData =
--       User \<$\> lookupInput "username" multipartData
--            \<*\> fmap fdPayload (lookupFile "pic" multipartData)
--
--   type API = MultipartForm Tmp User :> Post '[PlainText] String
--
--   server :: User -> Handler String
--   server usr = return str
--
--     where str = username usr ++ "'s profile picture"
--              ++ " got temporarily uploaded to "
--              ++ pic usr ++ " and will be removed from there "
--              ++ " after this handler has run."
--   @
--
--   Note that the behavior of this combinator is configurable,
--   by using 'serveWith' from servant-server instead of 'serve',
--   which takes an additional 'Context' argument. It simply is an
--   heterogeneous list where you can for example store
--   a value of type 'MultipartOptions' that has the configuration that
--   you want, which would then get picked up by servant-multipart.
--
--   __Important__: as mentionned in the example above,
--   the file paths point to temporary files which get removed
--   after your handler has run, if they are still there. It is
--   therefore recommended to move or copy them somewhere in your
--   handler code if you need to keep the content around.
data MultipartForm tag a

-- | What servant gets out of a @multipart/form-data@ form submission.
--
--   The type parameter 'tag' tells if 'MultipartData' is stored as a
--   temporary file or stored in memory. 'tag' is type of either 'Mem'
--   or 'Tmp'.
--
--   The 'inputs' field contains a list of textual 'Input's, where
--   each input for which a value is provided gets to be in this list,
--   represented by the input name and the input value. See haddocks for
--   'Input'.
--
--   The 'files' field contains a list of files that were sent along with the
--   other inputs in the form. Each file is represented by a value of type
--   'FileData' which among other things contains the path to the temporary file
--   (to be removed when your handler is done running) with a given uploaded
--   file's content. See haddocks for 'FileData'.
data MultipartData tag = MultipartData
  { inputs :: [Input]
  , files  :: [FileData tag]
  }

-- | Representation for an uploaded file, usually resulting from
--   picking a local file for an HTML input that looks like
--   @\<input type="file" name="somefile" /\>@.
data FileData tag = FileData
  { fdInputName :: Text     -- ^ @name@ attribute of the corresponding
                            --   HTML @\<input\>@
  , fdFileName  :: Text     -- ^ name of the file on the client's disk
  , fdFileCType :: Text     -- ^ MIME type for the file
  , fdPayload   :: MultipartResult tag
                            -- ^ path to the temporary file that has the
                            --   content of the user's original file. Only
                            --   valid during the execution of your handler as
                            --   it gets removed right after, which means you
                            --   really want to move or copy it in your handler.
  }

deriving instance Eq (MultipartResult tag) => Eq (FileData tag)
deriving instance Show (MultipartResult tag) => Show (FileData tag)

---- | Lookup a file input with the given @name@ attribute.
--lookupFile :: Text -> MultipartData tag -> Maybe (FileData tag)
--lookupFile iname = find ((==iname) . fdInputName) . files
--


-- | Representation for a textual input (any @\<input\>@ type but @file@).
--
--   @\<input name="foo" value="bar"\ />@ would appear as @'Input' "foo" "bar"@.
data Input = Input
  { iName  :: Text -- ^ @name@ attribute of the input
  , iValue :: Text -- ^ value given for that input
  } deriving (Eq, Show)

---- | Lookup a textual input with the given @name@ attribute.
--lookupInput :: Text -> MultipartData tag -> Maybe Text
--lookupInput iname = fmap iValue . find ((==iname) . iName) . inputs

-- | 'MultipartData' is the type representing
--   @multipart/form-data@ form inputs. Sometimes
--   you may instead want to work with a more structured type
--   of yours that potentially selects only a fraction of
--   the data that was submitted, or just reshapes it to make
--   it easier to work with. The 'FromMultipart' class is exactly
--   what allows you to tell servant how to turn "raw" multipart
--   data into a value of your nicer type.
--
--   @
--   data User = User { username :: Text, pic :: FilePath }
--
--   instance FromMultipart Tmp User where
--     fromMultipart form =
--       User \<$\> lookupInput "username" (inputs form)
--            \<*\> fmap fdPayload (lookupFile "pic" $ files form)
--   @
class FromMultipart tag a where
  -- | Given a value of type 'MultipartData', which consists
  --   in a list of textual inputs and another list for
  --   files, try to extract a value of type @a@. When
  --   extraction fails, servant errors out with status code 400.
  fromMultipart :: MultipartData tag -> Maybe a

instance FromMultipart tag (MultipartData tag) where
  fromMultipart = Just

-- | Allows you to tell servant how to turn a more structured type
--   into a 'MultipartData', which is what is actually sent by the
--   client.
--
--   @
--   data User = User { username :: Text, pic :: FilePath }
--
--   instance toMultipart Tmp User where
--       toMultipart user = MultipartData [Input "username" $ username user]
--                                        [FileData "pic"
--                                                  (pic user)
--                                                  "image/png"
--                                                  (pic user)
--                                        ]
--   @
class ToMultipart tag a where
  -- | Given a value of type 'a', convert it to a
  -- 'MultipartData'.
  toMultipart :: a -> MultipartData tag

instance ToMultipart tag (MultipartData tag) where
  toMultipart = id

-- | Given a bytestring for the boundary, turns a `MultipartData` into
-- a 'RequestBody'
multipartToBody :: forall tag. 
                MultipartBackend tag
                => LBS.ByteString
                -> MultipartData tag
                -> RequestBody
multipartToBody boundary mp = RequestBodySource $ files' <> source ["--", boundary, "--"]
  where
    -- at time of writing no Semigroup or Monoid instance exists for SourceT and StepT
    -- in releases of Servant; they are in master though
    (SourceT l) `mappend'` (SourceT r) = SourceT $ \k ->
                                                   l $ \lstep ->
                                                   r $ \rstep ->
                                                   k (appendStep lstep rstep)
    appendStep Stop        r = r
    appendStep (Error err) _ = Error err
    appendStep (Skip s)    r = appendStep s r
    appendStep (Yield x s) r = Yield x (appendStep s r)
    appendStep (Effect ms) r = Effect $ (flip appendStep r <$> ms)
    mempty' = SourceT ($ Stop)
    crlf = "\r\n"
    lencode = LBS.fromStrict . encodeUtf8
    renderInput input = renderPart (lencode . iName $ input) 
                                   "text/plain"
                                   ""
                                   (source . pure . lencode . iValue $ input)
    inputs' = foldl' (\acc x -> acc `mappend'` renderInput x) mempty' (inputs mp)
    renderFile :: FileData tag -> SourceIO LBS.ByteString
    renderFile file = renderPart (lencode . fdInputName $ file)
                                 (lencode . fdFileCType $ file)
                                 ((flip mappend) "\"" . mappend "; filename=\""
                                                      . lencode
                                                      . fdFileName $ file)
                                 (loadFile (Proxy @tag) . fdPayload $ file)
    files' = foldl' (\acc x -> acc `mappend'` renderFile x) inputs' (files mp)
    renderPart name contentType extraParams payload =
      source [ "--"
             , boundary
             , crlf
             , "Content-Disposition: form-data; name=\""
             , name
             , "\""
             , extraParams
             , crlf
             , "Content-Type: "
             , contentType
             , crlf
             , crlf
             ] `mappend'` payload `mappend'` source [crlf]


class MultipartBackend tag where
    type MultipartResult tag :: *
    type MultipartBackendOptions tag :: *

    backend :: Proxy tag
            -> MultipartBackendOptions tag
            -> InternalState
            -> ignored1
            -> ignored2
            -> IO SBS.ByteString
            -> IO (MultipartResult tag)

    loadFile :: Proxy tag -> MultipartResult tag -> SourceIO LBS.ByteString

    defaultBackendOptions :: Proxy tag -> MultipartBackendOptions tag

-- | Tag for data stored as a temporary file
data Tmp

-- | Tag for data stored in memory
data Mem


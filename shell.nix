with (import (builtins.fetchGit {
  name = "nixos-unstable-2020-01-20";
  url = https://github.com/nixos/nixpkgs/;
  rev = "bea1a232c615aba177e0ef56600d5f847ad3bbd9";
  }) {}).pkgs;

let
  haskell-packages = haskell.packages.ghc865;
  ghc = haskell-packages.ghcWithPackages
    (pkgs: with pkgs; [
      zlib
    ]);
in
  stdenv.mkDerivation {
    name = "dev-ghc";
    buildInputs = [
      ghc
      haskell-packages.cabal-install
    ];
    shellHook = ''
      export NIX_GHC_LIBDIR=$(ghc --print-libdir)
    '';
  }

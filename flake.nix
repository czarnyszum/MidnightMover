{
  description = "MidnightMover Haskell project";

  inputs = {
    # твоя ревизия nixpkgs
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        # используем Haskell-пакетный набор
        hpkg = pkgs.haskellPackages;

        # подставляем зависимости из hpkg
        midnightMover = hpkg.callPackage ./default.nix {
	  mkDerivation    = hpkg.mkDerivation;
          aeson           = hpkg.aeson;
          base            = hpkg.base;
          bytestring      = hpkg.bytestring;
          http-client     = hpkg.http-client;
          http-client-tls = hpkg.http-client-tls;
          lens            = hpkg.lens;
          mtl             = hpkg.mtl;
          tagsoup         = hpkg.tagsoup;
          text            = hpkg.text;
          wreq            = hpkg.wreq;
        };
      in
      {
        packages.default = midnightMover;

        apps.default = {
          type = "app";
          program = "${midnightMover}/bin/MidnightMover";
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            hpkg.ghc
            pkgs.cabal-install
            pkgs.haskell-language-server
          ];
        };
      });
}

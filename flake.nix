{
  description = "MidnightMover Haskell project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        # Используем default.nix, сгенерированный cabal2nix
        midnightMover = pkgs.callPackage ./default.nix { };
      in
      {
        packages = {
          # основной пакет, который соберется по `nix build`
          default = midnightMover;
        };

        apps.default = {
          type = "app";
          program = "${midnightMover}/bin/MidnightMover";
        };

        # dev-окружение `nix develop`
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.ghc
            pkgs.cabal-install
            pkgs.haskell-language-server
          ];
        };
      });
}
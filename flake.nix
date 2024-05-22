{
  inputs = {
    flake-utils.url     = "github:numtide/flake-utils";
    nixpkgs.url         = "github:NixOS/nixpkgs/nixos-unstable";
    crem.url            = "github:marcosh/crem";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells = {
          default = pkgs.mkShell {
            packages =
            let
              watchWithGhcid = pkgs.writers.writeDashBin "watch" ''
                ${pkgs.ghcid}/bin/ghcid --command="cabal repl"
              '';
              # Wrap cabal to always run `hpack` first.
              cabalWrapped = pkgs.writers.writeDashBin "cabal" ''
                ${pkgs.hpack}/bin/hpack >/dev/null 2>&1
                ${pkgs.cabal-install}/bin/cabal "$@"
              '';
              hask = pkgs.haskell.packages.ghc98.override {
              };
            in with pkgs; [
              cabalWrapped
              hpack
              ghcid
              stylish-haskell
              watchWithGhcid

              (hask.ghcWithPackages (ps: with ps; [
                HUnit
                QuickCheck
                bytestring
                containers
                async
                async-pool
                stm
              ]))
            ];
          };
        };
      }
    );
}



{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, ... }:
    let
      overlay = final: previous: {
        haskellPackages = previous.haskellPackages.override {
          overrides = haskellFinal: _: {
            "aoc" = haskellFinal.callCabal2nix "aoc" self { };
          };
        };
        aoc = final.haskell.lib.justStaticExecutables final.haskellPackages.aoc;
      };
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
      in {
        packages.default = pkgs.aoc;
        devShells = {
          default = pkgs.mkShell {
            name = "aoc-haskell-dev-shell";
            nativeBuildInputs = [
              (pkgs.buildPackages.haskellPackages.ghcWithPackages (pkgs: [ pkgs.cabal-install pkgs.haskell-language-server ]))
              (pkgs.writeShellScriptBin "run" ''
                set -xeu
                cabal run aoc -- "''${@:1}"
              '')
            ];
          };
          static = let
            extraLibDirs = (nixpkgs.lib.foldr (p: s: "${p} ${s}") "" 
              (map (p: "--extra-lib-dirs=${p}/lib") [
                pkgs.pkgsStatic.gmp6
                pkgs.pkgsStatic.libffi
                pkgs.pkgsStatic.zlib
              ])
            );
          in pkgs.mkShell {
            name = "static-hs-aoc-compile-shell";
            nativeBuildInputs = [
              (pkgs.pkgsMusl.buildPackages.haskellPackages.ghcWithPackages (pkgs: [ pkgs.cabal-install ]))
              pkgs.upx
              (pkgs.writeShellScriptBin "build" ''
                set -xeu
                cabal build --ghc-option=-optl=-static --ghc-option=-O2 --ghc-option=-w ${extraLibDirs}
                cp $(cabal list-bin aoc) ./aoc
                strip aoc
                upx aoc
              '')
            ];
          };
        };
      }
    ) // { inherit overlay; };
}


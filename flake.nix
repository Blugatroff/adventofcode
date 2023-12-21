{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        extraLibDirs = (nixpkgs.lib.foldr (p: s: "${p} ${s}") "" 
          (map (p: "--extra-lib-dirs=${p}/lib") [
            pkgs.pkgsStatic.gmp6
            pkgs.pkgsStatic.libffi
            pkgs.pkgsStatic.zlib
          ])
        );
      in
      {
        devShells = {
          default = pkgs.mkShell {
            name = "static-haskell-shell";
            nativeBuildInputs = [
              (pkgs.pkgsMusl.buildPackages.haskellPackages.ghcWithPackages (pkgs: [ pkgs.cabal-install ]))
              pkgs.cabal-install
              pkgs.haskell-language-server
              pkgs.upx
            ];
            shellHook = ''
              build() {
                cabal build --ghc-option=-optl=-static --ghc-option=-O2 --ghc-option=-w ${extraLibDirs}
                cp $(cabal list-bin aoc) ./aoc
                strip aoc
                upx aoc
              }
              run() {
                cabal run aoc --ghc-option=-optl=-static ${extraLibDirs} -- "''${@:1}"
              }
            '';
          };
       };
     }
  );
}


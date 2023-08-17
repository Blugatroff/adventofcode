{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells = {
          default = pkgs.mkShell {
            name = "haskell-cabal-shell";
            buildInputs = [
              (pkgs.haskellPackages.ghcWithPackages (pkgs: [ pkgs.cabal-install pkgs.haskell-language-server ]))
              pkgs.just
            ];
          };
       };
     }
  );
}


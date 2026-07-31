{
  description = "domaindriven dev shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ghc = pkgs.haskell.compiler.ghc9124;
      in
      {
        devShells.default = pkgs.mkShell.override { stdenv = pkgs.stdenvNoCC; } {
          nativeBuildInputs = [
            pkgs.pkg-config
          ];

          buildInputs = [
            ghc
            pkgs.cabal-install
            pkgs.process-compose
            pkgs.haskell.packages.ghc9124.ghcid
            pkgs.haskell.packages.ghc9124.haskell-language-server
            pkgs.haskell.packages.ghc9124.hspec-discover
            pkgs.zlib.dev
            pkgs.gmp.dev
            pkgs.xz.dev
            pkgs.libpq
          ];

          env = {
            PKG_CONFIG_PATH = pkgs.lib.makeSearchPath "lib/pkgconfig" [
              pkgs.xz.dev
              pkgs.zlib.dev
            ];
            C_INCLUDE_PATH = pkgs.lib.makeSearchPath "include" [
              pkgs.xz.dev
              pkgs.zlib.dev
              pkgs.gmp.dev
              pkgs.libpq.dev
            ];
            LIBRARY_PATH = pkgs.lib.makeLibraryPath [
              pkgs.xz.out
              pkgs.zlib.out
              pkgs.gmp.out
              pkgs.libpq.out
            ];
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
              pkgs.xz.out
              pkgs.zlib.out
              pkgs.gmp.out
              pkgs.libpq.out
            ];
          };
        };
      });
}

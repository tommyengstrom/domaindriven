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
        # Checks for transitive dev-tool dependencies can hang under GHC 9.12.4
        # (notably QuickCheck's terminal test in Nix's headless sandbox). This
        # affects only how Nix builds the shell tools; project tests still run
        # through Cabal and process-compose.
        haskellPackages = pkgs.haskell.packages.ghc9124.override {
          overrides = hfinal: hprev: {
            mkDerivation = args: hprev.mkDerivation (args // { doCheck = false; });
          };
        };
      in
      {
        devShells.default = pkgs.mkShell.override { stdenv = pkgs.stdenvNoCC; } {
          buildInputs = [
            ghc
            pkgs.cabal-install
            pkgs.curl
            pkgs.git
            pkgs.pkg-config
            pkgs.libpq.pg_config
            pkgs.process-compose
            haskellPackages.ghcid
            # HLS is intentionally omitted: its GHC 9.12.4 profiling build
            # currently triggers an upstream compiler panic.
            haskellPackages.hspec-discover
            haskellPackages.implicit-hie
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

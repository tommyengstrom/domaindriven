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
        ghc = pkgs.haskell.compiler.ghc9103;
      in
      {
        devShells.default = pkgs.mkShell.override { stdenv = pkgs.stdenvNoCC; } {
          nativeBuildInputs = [
            pkgs.pkg-config
          ];

          buildInputs = [
            pkgs.fish
            ghc
            pkgs.cabal-install
            pkgs.haskell.packages.ghc9103.haskell-language-server
            pkgs.haskell.packages.ghc9103.hspec-discover
            pkgs.zlib.dev
            pkgs.gmp.dev
            pkgs.xz.dev
            pkgs.libpq
          ];

          SHELL = "${pkgs.fish}/bin/fish";

          shellHook = ''
            export PKG_CONFIG_PATH="${pkgs.xz.dev}/lib/pkgconfig:${pkgs.zlib.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
            export LIBRARY_PATH="${pkgs.xz.out}/lib:${pkgs.zlib.out}/lib:${pkgs.gmp.out}/lib:${pkgs.libpq.out}/lib:$LIBRARY_PATH"
            export LD_LIBRARY_PATH="${pkgs.xz.out}/lib:${pkgs.zlib.out}/lib:${pkgs.gmp.out}/lib:${pkgs.libpq.out}/lib:$LD_LIBRARY_PATH"
            exec ${pkgs.fish}/bin/fish
          '';
        };
      });
}

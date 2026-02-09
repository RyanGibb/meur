{
  description = "Meur - A static site generator with bibliography and photo support";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Build the meur package from the cabal file
        meur = pkgs.haskellPackages.callCabal2nix "meur" ./. { };

        # Haskell development environment with tools
        haskell-env = pkgs.haskellPackages.ghcWithHoogle (hp: with hp; [
          haskell-language-server
          cabal-install
        ] ++ meur.buildInputs);

      in
      {
        packages = {
          default = meur;
          inherit meur;
        };

        apps.default = {
          type = "app";
          program = "${meur}/bin/meur";
        };

        devShells.default = pkgs.mkShell {
          name = "meur-dev";
          buildInputs = [
            haskell-env
            pkgs.imagemagick  # For thumbnail generation
            pkgs.curl         # For geocoding
          ];
          shellHook = ''
            export HAKYLL_ENV="development"
            export HIE_HOOGLE_DATABASE="${haskell-env}/share/doc/hoogle/default.hoo"
            export NIX_GHC="${haskell-env}/bin/ghc"
            export NIX_GHCPKG="${haskell-env}/bin/ghc-pkg"
            export NIX_GHC_DOCDIR="${haskell-env}/share/doc/ghc/html"
            export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )

            echo "Meur development environment loaded!"
            echo "Available commands:"
            echo "  cabal build        - Build the project"
            echo "  cabal run meur     - Run the meur CLI"
            echo "  cabal repl         - Start a REPL"
          '';
        };
      }
    );
}

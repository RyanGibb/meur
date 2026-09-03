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

        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            # Cache compiled regexes in Pattern matching; Hakyll recompiles
            # them on every match call, which dominates regex-heavy builds.
            hakyll = pkgs.haskell.lib.appendPatches super.hakyll
              [ ./nix/hakyll-regex-cache.patch ];
          };
        };

        # Build the meur package from the cabal file
        meur = haskellPackages.callCabal2nix "meur" ./. { };

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

        devShells.default = haskellPackages.shellFor {
          packages = p: [ meur ];
          withHoogle = true;
          nativeBuildInputs = with haskellPackages; [
            haskell-language-server
            cabal-install
          ];
          buildInputs = [
            pkgs.imagemagick  # For thumbnail generation
            pkgs.curl         # For geocoding
          ];
          shellHook = ''
            export HAKYLL_ENV="development"

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

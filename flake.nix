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

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ meur ];
          withHoogle = true;
          nativeBuildInputs = with pkgs.haskellPackages; [
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

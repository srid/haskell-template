{
  description = "haskell-template's description";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/3eb07eeafb52bcbf02ce800f032f18d666a9498d";
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-compat.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # Because: https://zimbatm.com/notes/1000-instances-of-nixpkgs
        pkgs = nixpkgs.legacyPackages.${system};

        # Change GHC version here. To get the appropriate value, run:
        #   nix-env -f "<nixpkgs>" -qaP -A haskell.compiler
        hp = pkgs.haskellPackages; # pkgs.haskell.packages.ghc921;

        project = returnShellEnv:
          hp.developPackage {
            inherit returnShellEnv;
            name = "haskell-template";
            root = ./.;
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              # Use callCabal2nix to override Haskell dependencies here
              # cf. https://tek.brick.do/K3VXJd8mEKO7
              # Example: 
              # > NanoID = self.callCabal2nix "NanoID" inputs.NanoID { };
              # Assumes that you have the 'NanoID' flake input defined.
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with hp; pkgs.lib.lists.optionals returnShellEnv [
                # Specify your build/dev dependencies here. 
                cabal-fmt
                cabal-install
                ghcid
                haskell-language-server
                ormolu
                pkgs.nixpkgs-fmt
              ]);
          };
      in
      {
        # Used by `nix build` & `nix run` (prod exe)
        defaultPackage = project false;

        # Used by `nix develop` (dev shell)
        devShell = project true;
      });
}

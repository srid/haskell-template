{
  description = "haskell-template's description";
  inputs = {
    # To find a suitable nixpkgs hash with cache, pick one from https://status.nixos.org/
    nixpkgs.url = "github:nixos/nixpkgs/1ec61dd4167f04be8d05c45780818826132eea0d";
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-compat.inputs.nixpkgs.follows = "nixpkgs";
    lint-utils = {
      type = "git";
      url = "https://gitlab.homotopic.tech/nix/lint-utils.git";
      ref = "lc/fourmolu";
      inputs.nixpkgs.follows = "nixpkgs";
    }; 
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
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
                  fourmolu
                  hlint
                  pkgs.nixpkgs-fmt
                ]);
            };
        in
        {
          # Used by `nix build` & `nix run` (prod exe)
          defaultPackage = project false;
          # Used by `nix develop` (dev shell)
          devShell = project true;

          # Used by `nix run ...`
          apps = {
            format = inputs.lint-utils.apps.${system}.fourmoluStandard8107;
          };

          # Used by `nix flake check`
          checks = {
            format-haskell = inputs.lint-utils.linters.${system}.fourmoluStandardGHC8107;
            format-cabal = inputs.lint-utils.linters.${system}.cabal;
            format-nix = inputs.lint-utils.linters.${system}.nixpkgs-fmt;
          };
          check = 
            pkgs.runCommand "combined-checks"
              {
                checksss = builtins.attrValues self.checks.${system};
              } ''
              echo $checksss
              touch $out
            '' ;

        }) // {
      # For hercules-CI support, 
      # https://docs.hercules-ci.com/hercules-ci/guides/upgrade-to-agent-0.9/#_upgrade_your_repositories
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}

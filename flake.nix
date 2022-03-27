{
  description = "haskell-template's description";
  inputs = {
    # To find a suitable nixpkgs hash with cache, pick one from https://status.nixos.org/
    nixpkgs.url = "github:nixos/nixpkgs/4d60081494259c0785f7e228518fee74e0792c1b";
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-compat.inputs.nixpkgs.follows = "nixpkgs";
    lint-utils = {
      type = "git";
      url = "https://gitlab.homotopic.tech/nix/lint-utils.git";
      ref = "master";
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
          haskellFormatter = "fourmoluStandardGhc8107"; # The formatter to use from inputs.lint-utils

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

          # Checks the shell script using ShellCheck
          checkedShellScript = name: text:
            (pkgs.writeShellApplication {
              inherit name text;
            }) + "/bin/${name}";

          # Concat a list of Flake apps to produce a new app that runs all of them
          # in sequence.
          concatApps = apps:
            {
              type = "app";
              program = checkedShellScript "concatApps"
                (pkgs.lib.strings.concatMapStringsSep
                  "\n"
                  (app: app.program)
                  apps);
            };

        in
        {
          # Used by `nix build` & `nix run` (prod exe)
          defaultPackage = project false;
          # Used by `nix develop` (dev shell)
          devShell = project true;

          # Used by `nix run ...`
          apps = {
            format = concatApps [
              inputs.lint-utils.apps.${system}.${haskellFormatter}
              inputs.lint-utils.apps.${system}.cabal-fmt
              inputs.lint-utils.apps.${system}.nixpkgs-fmt
            ];
          };

          # Used by `nix flake check` (but see next attribute)
          checks = {
            format-haskell = inputs.lint-utils.linters.${system}.${haskellFormatter} ./.;
            format-cabal = inputs.lint-utils.linters.${system}.cabal-fmt ./.;
            format-nix = inputs.lint-utils.linters.${system}.nixpkgs-fmt ./.;
            hls = checkedShellScript "hls" "${hp.haskell-language-server}/bin/haskell-language-server";
          };

          # We need this hack because `nix flake check` won't work for Haskell
          # projects: https://nixos.wiki/wiki/Import_From_Derivation#IFD_and_Haskell
          #
          # Instead, run: `nix build .#check.x86_64-linux` (replace with your system)
          check =
            pkgs.runCommand "combined-checks"
              {
                checksss = builtins.attrValues self.checks.${system};
              } ''
              echo $checksss
              touch $out
            '';

        }) // {
      # For hercules-CI support, 
      # https://docs.hercules-ci.com/hercules-ci/guides/upgrade-to-agent-0.9/#_upgrade_your_repositories
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}

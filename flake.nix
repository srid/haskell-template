{
  description = "haskell-template's description";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/v1.0.0";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-compat.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs:
    let
      # Function that produces Flake outputs for the given system.
      #
      # We use eachDefaultSystem (see below) to allow other architectures.
      # cf. https://github.com/NixOS/nix/issues/3843#issuecomment-661720562
      outputsFor = system:
        let
          # Because: https://zimbatm.com/notes/1000-instances-of-nixpkgs
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (pkgs.lib.lists) optionals;

          # Specify GHC version here. To get the appropriate value, run:
          #   nix-env -f "<nixpkgs>" -qaP -A haskell.compiler
          hp = pkgs.haskellPackages; # Eg: pkgs.haskell.packages.ghc921;

          # Specify your build/dev dependencies here.
          shellDeps = with hp; [
            cabal-fmt
            cabal-install
            ghcid
            haskell-language-server
            fourmolu
            hlint
            pkgs.nixpkgs-fmt
            pkgs.treefmt
          ];

          project =
            { returnShellEnv ? false
            , withHoogle ? false
            }:
            hp.developPackage {
              inherit returnShellEnv withHoogle;
              name = "haskell-template";
              root = ./.;
              overrides = self: super: with pkgs.haskell.lib; {
                # Use callCabal2nix to override Haskell dependencies here
                # cf. https://tek.brick.do/K3VXJd8mEKO7
                # Example: 
                # > NanoID = self.callCabal2nix "NanoID" inputs.NanoID { };
                # Assumes that you have the 'NanoID' flake input defined.
              };
              modifier = drv:
                pkgs.haskell.lib.overrideCabal drv (oa: {
                  # All the Cabal-specific overrides go here.
                  # For examples on what is possible, see:
                  #   https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib/compose.nix
                  buildTools = (oa.buildTools or [ ]) ++ optionals returnShellEnv shellDeps;
                });
            };
        in
        {
          # Used by `nix build ...`
          packages = {
            default = project { };
          };
          # Used by `nix run ...`
          apps = {
            default = {
              type = "app";
              program = "${inputs.self.packages.${system}.default}/bin/haskell-template";
            };
          };
          # Used by `nix develop ...`
          devShells = {
            default = project { returnShellEnv = true; withHoogle = true; };
          };
          # For compatability with older Nix (eg in CI)
          devShell = inputs.self.devShells.${system}.default;
          defaultPackage = inputs.self.packages.${system}.default;
          defaultApp = inputs.self.apps.${system}.default;
        };
    in
    inputs.flake-utils.lib.eachDefaultSystem outputsFor
    // {
      # For hercules-CI support, 
      # https://docs.hercules-ci.com/hercules-ci/guides/upgrade-to-agent-0.9/#_upgrade_your_repositories
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}

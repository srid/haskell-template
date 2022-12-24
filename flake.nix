{
  description = "srid/haskell-template: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake/checks-as-drv";
    treefmt-nix.url = "github:Platonic-Systems/treefmt-nix/flake-module"; # https://github.com/numtide/treefmt-nix/pull/14
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";

    # CI
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ... }: {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule

        # CI
        inputs.hercules-ci-effects.flakeModule
      ];
      perSystem = { self', lib, config, pkgs, ... }: {
        # The "main" project. You can have multiple projects, but this template
        # has only one.
        haskellProjects.main = {
          packages = {
            haskell-template.root = ./.;
          };
          buildTools = hp: {
            treefmt = config.treefmt.build.wrapper;
          } // config.treefmt.build.programs;
          # overrides = self: super: {}
          hlsCheck.enable = false;
          hlintCheck.enable = true;
        };

        # Auto formatters. This also adds a flake check to ensure that the
        # source tree was auto formatted.
        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };
        };

        # Dev shell scripts.
        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            exec = ''
              echo http://127.0.0.1:8888
              hoogle serve -p 8888 --local
            '';
            category = "Dev Tools";
          };
          repl = {
            description = "Start the cabal repl";
            exec = ''
              cabal repl "$@"
            '';
            category = "Dev Tools";
          };
          fmt = {
            description = "Format the source tree";
            exec = "${lib.getExe config.treefmt.build.wrapper}";
            category = "Dev Tools ";
          };
          run = {
            description = "Run the project with ghcid auto-recompile";
            exec = ''
              ghcid -c "cabal repl exe:haskell-template" --warnings -T :main
            '';
            category = "Primary";
          };
        };

        # Default package.
        packages.default = self'.packages.main-haskell-template;

        # Default shell.
        devShells.default =
          config.mission-control.installToDevShell self'.devShells.main;

        # checks.hlsCheck = config.haskellProjects.main.hlsCheck.drv;
      };

      herculesCI.ciSystems = [ "x86_64-linux" "aarch64-darwin" ];
      # CI configuration
      flake = {
        effects =
          withSystem "x86_64-linux" (
            { config, hci-effects, pkgs, ... }:
            {
              # hlsCheck = config.haskellProjects.main.hlsCheck.drv;
              hlsCheck = hci-effects.mkEffect {
                effectScript = ''
                  cp -R ${self} $HOME/project
                  chmod -R a+w $HOME/project
                  pushd $HOME/project
                  pwd
                  ls -l
                  nix develop -c haskell-language-server
                '';
              };
            }
          );
      };
    });
}

{
  description = "srid/haskell-template: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
        ./nix/treefmt-flake/flake-module.nix
      ];
      perSystem = { self', lib, config, pkgs, ... }: {
        # The "main" project. You can have multiple projects, but this template
        # has only one.
        haskellProjects.main = {
          packages = {
            haskell-template.root = ./.;
          };
          buildTools = hp: {
            treefmt = config.treefmt.wrapper;
          } // config.treefmt.programs;
          # overrides = self: super: {}
          hlsCheck.enable = true;
          hlintCheck.enable = true;
        };

        treefmt.config = {
          package = pkgs.treefmt;
          projectRootFile = "flake.nix";

          programs.ormolu.enable = true;
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };

          programs.nixpkgs-fmt.enable = true;

          programs.cabal-fmt.enable = true;
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
            exec = "${lib.getExe config.treefmt.wrapper}";
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
      };
    };
}

{
  description = "srid/haskell-template: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-flake.url = "github:srid/treefmt-flake";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-flake.flakeModule
        ./nix/script-module.nix
      ];
      perSystem = { self', lib, config, pkgs, ... }: {
        haskellProjects.project = {
          packages = {
            haskell-template.root = ./.;
          };
          buildTools = hp: {
            inherit (pkgs)
              treefmt;
          } // config.treefmt.formatters;
          # overrides = self: super: {}
          hlsCheck.enable = true;
          hlintCheck.enable = true;
        };
        treefmt.formatters = {
          inherit (pkgs)
            nixpkgs-fmt;
          inherit (pkgs.haskellPackages)
            cabal-fmt
            fourmolu;
        };
        script.scripts = {
          # TODO: Use lib.getExe to substitute the key, so we can call this
          # "hoogle" without conflicting with existing build tools.
          hoog = {
            description = "Start Hoogle server for project dependencies";
            command = ''
              echo http://127.0.0.1:8888
              hoogle serve -p 8888 --local
            '';
            category = "Dev Tools";
          };
          repl = {
            description = "Start the cabal repl";
            command = ''
              cabal repl "$@"
            '';
            category = "Dev Tools";
          };
          run = {
            description = "Run the project with ghcid auto-recompile";
            command = ''
              ghcid -c "cabal repl exe:haskell-template" --warnings -T :main
            '';
            category = "Primary";
          };
        };
        packages.default = self'.packages.project-haskell-template;
        devShells.default =
          config.script.installToDevShell config.devShells.project;
      };
    };
}



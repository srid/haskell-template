{
  description = "srid/haskell-template: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      # See ./nix/modules/*.nix for the modules that are imported here.
      imports =
        with builtins; map
          (fn: ./nix/modules/${fn})
          (attrNames (readDir ./nix/modules));

      perSystem = { self', lib, config, pkgs, ... }: {
        # Default shell.
        devShells.default = pkgs.mkShell {
          name = "haskell-template";
          meta.description = "Haskell development environment";
          # See https://community.flake.parts/haskell-flake/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell # See ./nix/modules/haskell.nix
            config.treefmt.build.devShell # See ./nix/modules/formatter.nix
          ];
          packages = with pkgs; [
            just
            nixd
          ];
        };
      };
    };
}

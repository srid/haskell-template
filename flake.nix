{
  description = "Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      imports = [
        inputs.flake-parts.flakeModules.partitions
        ./haskell.nix
      ];

      # Partition development outputs to avoid fetching dev-only inputs
      # when users only need packages/apps
      partitionedAttrs = {
        devShells = "dev";
        checks = "dev";
      };

      partitions.dev = {
        extraInputsFlake = ./dev;
        module = {
          imports = [
            ./dev/flake-module.nix
          ];
        };
      };
    };
}

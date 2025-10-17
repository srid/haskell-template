# Partition configuration for development outputs
# This routes devShells and checks to the dev partition
{ inputs, ... }:

{
  imports = [
    inputs.flake-parts.flakeModules.partitions
  ];
  # Partition development outputs to avoid fetching dev-only inputs
  # when users only need packages/apps
  partitionedAttrs = {
    devShells = "dev";
    checks = "dev";
  };

  partitions.dev = {
    extraInputsFlake = ./.;
    module = {
      imports = [
        ./flake-module.nix
      ];
    };
  };
}

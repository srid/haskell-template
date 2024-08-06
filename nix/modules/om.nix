# Configuration for https://omnix.page/
{ inputs, ... }:

{
  imports = [
    inputs.omnix-flake.flakeModules.default
  ];
  flake = {
    om = {
      health.default = {
        nix-version.min-required = "2.17.0";
        system = {
          min_ram = "8G";
        };
      };
    };
  };
  perSystem = { inputs', system, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [
        (self: super: {
          omnix = inputs'.omnix.packages.default;
        })
      ];
    };
  };
}

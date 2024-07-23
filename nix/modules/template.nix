{ inputs, ... }:

{
  imports = [
    inputs.treefmt-nix.flakeModule
    inputs.fourmolu-nix.flakeModule
  ];
  flake.templates.default = {
    description = "A batteries-included Haskell project template for Nix";
    path = builtins.path { path = inputs.self; };
  };
}

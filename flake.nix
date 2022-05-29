{
  description = "haskell-template's description";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-compat.inputs.nixpkgs.follows = "nixpkgs";
  };

  # We use flake-parts as a way to make flakes 'system-aware'
  # cf. https://github.com/NixOS/nix/issues/3843#issuecomment-661720562
  outputs = { self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        ./haskell.nix
      ];
      perSystem = { ... }: {
        haskellProject = {
          name = "haskell-template";
        };
      };
    };
}

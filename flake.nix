{
  description = "srid/haskell-template: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = { self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          root = ./.;
          buildTools = hp: {
            # TODO: Use https://github.com/numtide/treefmt/pull/169
            inherit (pkgs)
              treefmt
              nixpkgs-fmt;
            inherit (hp)
              cabal-fmt
              fourmolu;
          };
        };
      };
    };
}

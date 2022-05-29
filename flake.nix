{
  description = "srid/haskell-template: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-compat.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        ./haskell.nix
      ];
      perSystem = { system, self', pkgs, ... }: {
        haskellProjects.haskell-template = {
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
        packages.default = self'.packages.haskell-template;
        apps.default = self'.apps.haskell-template;
        devShells.default = self'.devShells.haskell-template;
      };
    };
}

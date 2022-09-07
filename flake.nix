{
  description = "srid/haskell-template: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake/srid/break-developPackage";
    treefmt-flake.url = "github:srid/treefmt-flake";
    check-flake.url = "github:srid/check-flake";

    horizon-platform.url = "git+https://gitlab.homotopic.tech/horizon/horizon-platform";
    all-cabal-hashes.url = "github:commercialhaskell/all-cabal-hashes?ref=hackage";
    all-cabal-hashes.flake = false;
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-flake.flakeModule
        inputs.check-flake.flakeModule
        # TODO: Is there an idiomatic way to specify custom overlays for `pkgs`
        # passed by flake-parts (rather than hacking around it like we do by
        # setting _module.args here)?
        ({
          perSystem = { inputs', system, ... }: {
            _module.args.pkgs = import nixpkgs {
              inherit system;
              overlays = [
                (final: prev: {
                  all-cabal-hashes = inputs.all-cabal-hashes;
                })
              ];
            };
          };
        })
      ];
      perSystem = { self', config, pkgs, system, ... }: {
        haskellProjects.default = {
          root = ./.;
          haskellPackages = pkgs.haskell.packages.ghc942;
          overrides = inputs.horizon-platform.overrides.${system}.ghc942;
          buildTools = hp: {
            inherit (pkgs)
              treefmt;
          } // config.treefmt.formatters;
          enableHLSCheck = true;
        };
        treefmt.formatters = {
          inherit (pkgs)
            nixpkgs-fmt;
          inherit (pkgs.haskellPackages)
            cabal-fmt
            fourmolu;
        };
      };
    };
}

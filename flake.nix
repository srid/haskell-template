{
  description = "srid/haskell-template: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-flake.url = "github:srid/treefmt-flake/treefmt-nix";
    treefmt-nix.url = "github:numtide/treefmt-nix"; # WHY?
    check-flake.url = "github:srid/check-flake";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-flake.flakeModule
        inputs.check-flake.flakeModule
      ];
      perSystem = { self', config, pkgs, ... }: {
        haskellProjects.default = {
          root = ./.;
          buildTools = hp:
            let
              treefmtConfig = (inputs.treefmt-nix.lib.evalModule pkgs config.treefmt).config;
              treefmtTools = {
                treefmt = treefmtConfig.build.wrapper;
              } // builtins.mapAttrs (_: v: v.package) treefmtConfig.programs;
            in
            treefmtTools // {
              # Your buildtools go here
            };
          # overrides = self: super: {}
          enableHLSCheck = true;
        };
        treefmt = {
          projectRootFile = "flake.nix";
          package = pkgs.treefmt;
          programs = {
            nixpkgs-fmt.enable = true;
            cabal-fmt.enable = true;
            # FIXME
            /* ormolu = {
              enable = true;
              package = pkgs.haskellPackages.fourmolu;
            }; */
          };
          /* settings.formatter.ormolu = {
            # command = pkgs.haskellPackages.fourmolu;
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ]; */
        };
      };
    };
}

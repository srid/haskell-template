{
  description = "Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    nixos-unified.url = "github:srid/nixos-unified";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
  };

  outputs = inputs:
    # https://nixos-unified.org/autowiring.html#flake-parts
    inputs.nixos-unified.lib.mkFlake
      { inherit inputs; root = ./.; };
}

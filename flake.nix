{
  description = "Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    nixos-unified.url = "github:srid/nixos-unified";
    haskell-flake.url = "github:rsrohitsingh682/haskell-flake/cache-cabal2nix";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";

    git-hooks.url = "github:rsrohitsingh682/git-hooks.nix/cabal2nix";
    git-hooks.flake = false;
  };

  outputs = inputs:
    # This will import ./nix/modules/flake-parts/*.nix
    # cf. https://nixos-unified.org/autowiring.html#flake-parts
    #
    # To write your own Nix, add or edit files in ./nix/modules/flake-parts/
    inputs.nixos-unified.lib.mkFlake
      { inherit inputs; root = ./.; };
}

{
  description = "Development inputs for haskell-template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.flake = false;
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = { ... }: { };
}

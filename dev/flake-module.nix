# Development-only flake-parts module
# This module is loaded in the "dev" partition and includes devShell, pre-commit hooks, etc.

{ inputs, ... }:
{
  imports = [
    (inputs.git-hooks + /flake-module.nix)
    inputs.fourmolu-nix.flakeModule
    ../haskell.nix
  ];

  perSystem = { config, pkgs, ... }: {
    # Development shell
    devShells.default = pkgs.mkShell {
      name = "haskell-template";
      meta.description = "Haskell development environment";

      # See https://community.flake.parts/haskell-flake/devshell#composing-devshells
      inputsFrom = [
        config.haskellProjects.default.outputs.devShell
        config.pre-commit.devShell
      ];

      # Development packages
      packages = with pkgs; [
        just
        nixd
        ghciwatch
      ];
    };

    # Git pre-commit hooks
    pre-commit.settings = {
      hooks = {
        nixpkgs-fmt.enable = true;
        cabal-fmt.enable = true;
        fourmolu = {
          enable = true;
          package = config.fourmolu.wrapper;
        };
        hlint.enable = true;
      };
    };

    fourmolu.settings = {
      indentation = 2;
      comma-style = "leading";
      record-brace-space = true;
      indent-wheres = true;
      import-export-style = "diff-friendly";
      respectful = true;
      haddock-style = "multi-line";
      newlines-between-decls = 1;
      extensions = [ "ImportQualifiedPost" ];
    };
  };
}

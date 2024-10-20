{ inputs, ... }:
{
  imports = [
    inputs.treefmt-nix.flakeModule
    inputs.fourmolu-nix.flakeModule
  ];
  perSystem = { config, pkgs, ... }: {
    # Auto formatters. This also adds a flake check to ensure that the
    # source tree was auto formatted.
    treefmt.config = {
      projectRootFile = "flake.nix";

      programs.fourmolu = {
        enable = true;
        package = config.fourmolu.wrapper;
      };
      programs.nixpkgs-fmt.enable = true;
      programs.cabal-fmt.enable = true;
      programs.hlint.enable = true;
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

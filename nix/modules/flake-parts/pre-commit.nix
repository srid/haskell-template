{ inputs, ... }:
{
  imports = [
    (inputs.git-hooks + /flake-module.nix)
    inputs.fourmolu-nix.flakeModule
  ];
  perSystem = { config, pkgs, ... }: {
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

    fourmolu = {
      package = pkgs.haskellPackages.fourmolu_0_16_2_0;
      settings = {
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
  };
}

{
  perSystem = { config, pkgs, ... }: {
    # Default shell.
    devShells.default = pkgs.mkShell {
      name = "haskell-template";
      meta.description = "Haskell development environment";
      # See https://community.flake.parts/haskell-flake/devshell#composing-devshells
      inputsFrom = [
        config.haskellProjects.default.outputs.devShell # See ./nix/modules/haskell.nix
        config.treefmt.build.devShell # See ./nix/modules/formatter.nix
        config.om.health.outputs.devShell
      ];
      packages = with pkgs; [
        just
        nixd
      ];
    };
  };
}

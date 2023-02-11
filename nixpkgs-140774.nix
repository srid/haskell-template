# Workaround for https://github.com/NixOS/nixpkgs/issues/140774
{ self, config, lib, ... }:

{
  flake.haskellFlakeProjectModules.fixNixpkgs140774 = { pkgs, ... }: {
    overrides =
      let
        disableSeparateBinOutput =
          pkgs.haskell.lib.compose.overrideCabal (_: { enableSeparateBinOutput = false; });
      in
      self: super: lib.optionalAttrs (pkgs.system == "aarch64-darwin") {
        ghcid = disableSeparateBinOutput super.ghcid;
        ormolu = disableSeparateBinOutput super.ormolu;
      };
  };
}

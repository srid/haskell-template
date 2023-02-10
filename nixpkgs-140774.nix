# Workaround for https://github.com/NixOS/nixpkgs/issues/140774
{ self, config, lib, ... }:

{
  perSystem = { self', system, lib, config, pkgs, ... }: {
    haskellFlakeProjectModules.fixNixpkgs140774 = {
      overrides =
        let
          disableSeparateBinOutput =
            pkgs.haskell.lib.compose.overrideCabal (_: { enableSeparateBinOutput = false; });
        in
        self: super: lib.optionalAttrs (system == "aarch64-darwin") {
          ghcid = disableSeparateBinOutput super.ghcid;
          ormolu = disableSeparateBinOutput super.ormolu;
        };
    };
  };
}

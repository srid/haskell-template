# A flake-parts module for Haskell cabal projects.
{ self, config, lib, flake-parts-lib, ... }:

let
  inherit (flake-parts-lib)
    mkPerSystemOption;
  inherit (lib)
    mkOption
    types;
  inherit (types)
    deferredModule;
in
{
  options = {
    perSystem = mkPerSystemOption
      ({ config, self', inputs', pkgs, system, ... }: {
        options.haskellFlakeNixpkgs140774Workaround = mkOption {
          type = deferredModule;
          default = {
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
      });

  };
}

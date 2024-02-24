# Use horizon package set for haskell-flake
{ inputs, ... }:
{ pkgs, ... }:
{
  basePackages = inputs.horizon-platform.legacyPackages.${pkgs.system};
  # Horizon maintains separate package set for devtools.
  defaults.devShell.tools = _:
    let devtools = inputs.horizon-devtools.legacyPackages.${pkgs.system};
    in {
      inherit (devtools) cabal-install ghcid haskell-language-server;
    };
}

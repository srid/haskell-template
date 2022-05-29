{ config, lib, flake-parts-lib, ... }:

let
  inherit (flake-parts-lib)
    mkSubmoduleOptions
    mkPerSystemOption;
in
{
  options = {
    # The primed versions (self', inputs') are same as the non-primed
    # versions, but with 'system' already applied.
    perSystem = mkPerSystemOption
      ({ config, self', inputs', pkgs, system, ... }:
        let
          inherit (pkgs.lib.lists) optionals;

          # Specify GHC version here. To get the appropriate value, run:
          #   nix-env -f "<nixpkgs>" -qaP -A haskell.compiler
          hp = pkgs.haskellPackages; # Eg: pkgs.haskell.packages.ghc921;

          # Specify your build/dev dependencies here.
          shellDeps = with hp; [
            cabal-fmt
            cabal-install
            ghcid
            haskell-language-server
            fourmolu
            hlint
            pkgs.nixpkgs-fmt
            pkgs.treefmt
          ];

          project =
            { returnShellEnv ? false
            , withHoogle ? false
            }:
            hp.developPackage {
              inherit returnShellEnv withHoogle;
              name = "haskell-template";
              root = ./.;
              overrides = self: super: with pkgs.haskell.lib; {
                # Use callCabal2nix to override Haskell dependencies here
                # cf. https://tek.brick.do/K3VXJd8mEKO7
                # Example: 
                # > NanoID = self.callCabal2nix "NanoID" inputs'.NanoID { };
                # Assumes that you have the 'NanoID' flake input defined.
              };
              modifier = drv:
                pkgs.haskell.lib.overrideCabal drv (oa: {
                  # All the Cabal-specific overrides go here.
                  # For examples on what is possible, see:
                  #   https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib/compose.nix
                  buildTools = (oa.buildTools or [ ]) ++ optionals returnShellEnv shellDeps;
                });
            };
        in
        {
          # Used by `nix build ...`
          packages = {
            default = project { };
          };
          # Used by `nix run ...`
          apps = {
            default = {
              type = "app";
              program = pkgs.lib.getExe self'.packages.default;
            };
          };
          # Used by `nix develop ...`
          devShells = {
            default = project { returnShellEnv = true; withHoogle = true; };
          };
        });
  };
}

# A flake-parts module for Haskell cabal projects.
{ config, lib, flake-parts-lib, ... }:

let
  inherit (flake-parts-lib)
    mkSubmoduleOptions
    mkPerSystemOption;
  inherit (lib)
    mkOption
    types;
in
{
  options = {
    perSystem = mkPerSystemOption
      ({ config, self', inputs', pkgs, system, ... }: {
        options.haskellProject = {
          haskellPackages = mkOption {
            type = types.anything;
            description = ''Which Haskell package set to use'';
            default = pkgs.haskellPackages;
          };
          name = mkOption {
            type = types.str;
            description = ''Name of the Cabal package'';
          };
          root = mkOption {
            type = types.path;
            description = ''Path to the Cabal project root'';
            default = ./.;
          };
          overrides = mkOption {
            type = types.anything;
            description = ''Overrides for the Cabal project'';
            default = self: super: { };
          };
          modifier = mkOption {
            type = types.anything;
            description = ''Modifier for the Cabal project'';
            default = drv: drv;
          };
          extraBuildTools = mkOption {
            type = types.anything;
            description = ''Extra tools to add to nix-shell'';
            default = hp: [ ];
          };
        };
      });
  };
  config = {
    perSystem = { config, self', inputs', pkgs, ... }:
      let
        cfg = config.haskellProject;
        inherit (pkgs.lib.lists) optionals;

        # Specify GHC version here. To get the appropriate value, run:
        #   nix-env -f "<nixpkgs>" -qaP -A haskell.compiler
        hp = cfg.haskellPackages; # pkgs.haskellPackages; # Eg: pkgs.haskell.packages.ghc921;

        buildTools = with hp; [
          cabal-fmt
          cabal-install
          ghcid
          haskell-language-server
          fourmolu
          hlint
          pkgs.nixpkgs-fmt
          pkgs.treefmt
        ] ++ (cfg.extraBuildTools hp);

        project =
          { returnShellEnv ? false
          , withHoogle ? false
          }:
          hp.developPackage {
            inherit returnShellEnv withHoogle;
            inherit (cfg) root name overrides;
            modifier = drv:
              cfg.modifier (pkgs.haskell.lib.overrideCabal drv (oa: {
                # All the Cabal-specific overrides go here.
                # For examples on what is possible, see:
                #   https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib/compose.nix
                buildTools = (oa.buildTools or [ ]) ++ optionals returnShellEnv buildTools;
              }));
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
      };
  };
}

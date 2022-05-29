# A flake-parts module for Haskell cabal projects.
{ self, config, lib, flake-parts-lib, ... }:

let
  inherit (flake-parts-lib)
    mkSubmoduleOptions
    mkPerSystemOption;
  inherit (lib)
    mkOption
    mkDefault
    types;
  inherit (types)
    functionTo
    raw;
in
{
  options = {
    perSystem = mkPerSystemOption
      ({ config, self', inputs', pkgs, system, ... }: {
        options.haskellProjects = mkOption {
          description = "Haskell projects";
          type = types.attrsOf (types.submodule {
            options = {
              haskellPackages = mkOption {
                type = types.attrsOf raw;
                description = ''
                  Which Haskell package set to use

                  You can effectively select the GHC version here. To get the appropriate value, run:
                    nix-env -f "<nixpkgs>" -qaP -A haskell.compiler
                '';
                default = pkgs.haskellPackages;
              };
              root = mkOption {
                type = types.path;
                description = ''Path to the Cabal project root'';
                default = "${self}";
              };
              overrides = mkOption {
                type = functionTo (functionTo (types.attrsOf raw));
                description = ''Overrides for the Cabal project'';
                default = self: super: { };
              };
              modifier = mkOption {
                type = functionTo types.package;
                description = ''
                  Modifier for the Cabal project

                  Typically you want to use `overrideCabal` to override various
                  attributes of Cabal project.
              
                  For examples on what is possible, see:
                  https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib/compose.nix
                '';
                default = drv: drv;
              };
              buildTools = mkOption {
                type = functionTo (types.attrsOf (types.nullOr types.package));
                description = ''Build tools for your Haskell package (available only in nix shell).'';
                default = hp: { };
                defaultText = ''Build tools useful for Haskell development are included by default.'';
              };
            };
          });
        };
      });
  };
  config = {
    perSystem = { config, self', inputs', pkgs, ... }:
      let
        projects =
          lib.mapAttrs
            (name: cfg:
              let
                inherit (pkgs.lib.lists) optionals;
                hp = cfg.haskellPackages;
                defaultBuildTools = with hp; {
                  inherit
                    cabal-install
                    haskell-language-server
                    ghcid
                    hlint;
                };
                buildTools = lib.attrValues (defaultBuildTools // cfg.buildTools hp);
                mkProject = { returnShellEnv ? false, withHoogle ? false }:
                  hp.developPackage {
                    inherit returnShellEnv withHoogle name;
                    inherit (cfg) root overrides;
                    modifier = drv:
                      cfg.modifier (pkgs.haskell.lib.overrideCabal drv (oa: {
                        buildTools = (oa.buildTools or [ ]) ++ optionals returnShellEnv buildTools;
                      }));
                  };
              in
              rec {
                package = mkProject { };
                app = { program = package; };
                devShell = mkProject { returnShellEnv = true; withHoogle = true; };
              }
            )
            config.haskellProjects;
        # Inject a 'default' attr if the attrset is a singleton set.
        withDefault = attrs:
          let xs = lib.attrValues attrs; in
          if builtins.length xs == 1
          then attrs // { default = mkDefault (builtins.head xs); }
          else attrs;
      in
      {
        packages =
          withDefault (lib.mapAttrs
            (_: project: project.package)
            projects);
        apps =
          withDefault (lib.mapAttrs
            (_: project: project.app)
            projects);
        devShells =
          withDefault (lib.mapAttrs
            (_: project: project.devShell)
            projects);
      };
  };
}

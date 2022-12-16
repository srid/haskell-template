{ self, lib, flake-parts-lib, ... }:
let
  inherit (flake-parts-lib)
    mkPerSystemOption;
  inherit (lib)
    mkOption
    types;
in
{
  options = {
    perSystem = mkPerSystemOption
      ({ config, self', inputs', pkgs, system, ... }:
        let
          scriptSubmodule = types.submodule {
            options = {
              description = mkOption {
                type = types.nullOr types.str;
                description = "The description of this script";
                default = null;
              };
              category = mkOption {
                type = types.str;
                description = "The category of the command";
                default = "Commands";
              };
              # The following are enum options
              command = mkOption {
                type = types.nullOr types.str;
                description = "The command to run (if 'package' is not defined)";
                default = null;
              };
              package = mkOption {
                type = types.nullOr types.package;
                description = "The package to run";
                default = null;
              };
            };
          };

          mainSubmodule = types.submodule {
            options = {
              wrapperName = mkOption {
                type = types.str;
                description = lib.mdDoc "The name of the wrapper script";
                default = ",";
              };
              scripts = mkOption {
                type = types.attrsOf scriptSubmodule;
                description = lib.mdDoc ''
                  Scripts to be added to the shell
                '';
              };
              # Functions
              installToDevShell = mkOption {
                type = types.functionTo types.raw;
                description = lib.mdDoc ''
                  Patch a devshell to add scripts
                '';
                default = shell: shell.overrideAttrs (oa:
                  let
                    wrapper = import ./wrapper.nix { inherit pkgs lib; inherit (config) script; };
                    banner = import ./banner.nix { inherit wrapper; inherit (config.script) wrapperName; };
                  in
                  {
                    nativeBuildInputs = (oa.nativeBuildInputs or [ ]) ++ [ wrapper ];
                    shellHook = (oa.shellHook or "") + banner;
                  });
              };
            };
          };

        in
        {
          options.script = lib.mkOption {
            type = mainSubmodule;
            description = lib.mdDoc ''
              Specification for the scripts in dev shell
            '';
          };
        });
  };
}

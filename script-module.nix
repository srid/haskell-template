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
                type = types.str;
                description = "The description of this script";
              };
              category = mkOption {
                type = types.str;
                description = "The category of the command";
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
                default =
                  let
                    mkCommand = name: v:
                      (if v.package == null then pkgs.writeShellApplication { inherit name; text = v.command; } else v.package).overrideAttrs (oa: {
                        meta.description =
                          if v ? description then v.description else oa.meta.description or "No description";
                        meta.category = v.category or "Commands";
                      });
                    wrapCommands = spec:
                      let
                        commands = lib.mapAttrsToList mkCommand spec;
                        commandsGrouped = lib.groupBy (a: a.meta.category) commands;
                      in
                      pkgs.writeShellApplication {
                        name = ",";
                        runtimeInputs = commands;
                        text = ''
                          showHelp () {
                            echo -e "Available commands:\n"
                            ${
                              lib.concatStringsSep "echo;"
                                (lib.mapAttrsToList (cat: commands: 
                                  "echo -e '## " + cat + "';echo;" + 
                                    "echo '" + lib.concatStringsSep "\n" 
                                      (map (drv: 
                                        let name = builtins.baseNameOf (lib.getExe drv);
                                            desc = drv.meta.description;
                                        in "  " + name + "\t: " + desc
                                      ) commands 
                                      ) + "' | ${pkgs.util-linux}/bin/column -t -s ''$'\t'; "
                                ) commandsGrouped)
                            }
                          }
                          if [ "$*" == "" ] || [ "$*" == "-h" ] || [ "$*" == "--help" ]; then
                            showHelp
                            exit 1
                          else 
                            echo "Running command: $*"
                            exec "$@"
                          fi
                        '';
                      };
                  in
                  shell:
                  shell.overrideAttrs (oa: {
                    # TODO: Banner?
                    shellHook = (oa.shellHook or "") + ''
                '';
                    nativeBuildInputs = (oa.nativeBuildInputs or [ ]) ++ [
                      (
                        (wrapCommands config.script.scripts).overrideAttrs (_oa: {
                          meta.description = "Development scripts command";
                          nativeBuildInputs = (oa.nativeBuildInputs or [ ]) ++ [ pkgs.installShellFiles ];
                          # TODO: bash and zsh completion
                          postInstall = (oa.postInstall or "") + ''
                      '';
                        })
                      )
                    ];
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
  config = {
    perSystem = { config, self', inputs', pkgs, ... }: { };
  };
}



  

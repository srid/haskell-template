{ pkgs, lib, script, ... }:

let
  mkCommand = name: v:
    (if v.package == null then pkgs.writeShellApplication { inherit name; text = v.command; } else v.package).overrideAttrs (oa: {
      meta.description =
        if v.description == null then oa.meta.description or "No description" else v.description;
      meta.category = v.category;
    });
  wrapCommands = spec:
    let
      commands = lib.mapAttrsToList mkCommand spec;
      commandsGrouped = lib.groupBy (a: a.meta.category) commands;
    in
    pkgs.writeShellApplication {
      name = script.wrapperName;
      runtimeInputs = commands;
      # TODO: find_up!
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
                      in "  ${script.wrapperName} " + name + "\t: " + desc
                    ) commands 
                    ) + "' | ${lib.getExe pkgs.unixtools.column} -t -s ''$'\t'; "
              ) commandsGrouped)
          }
        }
        if [ "$*" == "" ] || [ "$*" == "-h" ] || [ "$*" == "--help" ]; then
          showHelp
          exit 1
        else 
          exec "$@"
        fi
      '';
    };
  wrapper =
    (wrapCommands script.scripts).overrideAttrs (oa: {
      meta.description = "Development scripts command";
      nativeBuildInputs = (oa.nativeBuildInputs or [ ]) ++ [ pkgs.installShellFiles ];
      # TODO: bash and zsh completion
      postInstall = (oa.postInstall or "") + ''
      '';
    });
in
wrapper

{ pkgs, lib, ... }:
let
  mkCommand = name: v:
    (v.package or pkgs.writeShellApplication { inherit name; text = v.command; }).overrideAttrs (oa: {
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
shell: spec:
shell.overrideAttrs (oa: {
  # TODO: Banner?
  shellHook = (oa.shellHook or "") + ''
                '';
  nativeBuildInputs = (oa.nativeBuildInputs or [ ]) ++ [
    (
      (wrapCommands spec).overrideAttrs (_oa: {
        meta.description = "Development scripts command";
        nativeBuildInputs = (oa.nativeBuildInputs or [ ]) ++ [ pkgs.installShellFiles ];
        # TODO: bash and zsh completion
        postInstall = (oa.postInstall or "") + ''
                      '';
      })
    )
  ];
})

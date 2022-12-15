{
  description = "srid/haskell-template: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-flake.url = "github:srid/treefmt-flake";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-flake.flakeModule
      ];
      perSystem = { self', lib, config, pkgs, ... }: {
        haskellProjects.project = {
          packages = {
            haskell-template.root = ./.;
          };
          buildTools = hp: {
            inherit (pkgs)
              treefmt;
          } // config.treefmt.formatters;
          # overrides = self: super: {}
          hlsCheck.enable = true;
          hlintCheck.enable = true;
        };
        treefmt.formatters = {
          inherit (pkgs)
            nixpkgs-fmt;
          inherit (pkgs.haskellPackages)
            cabal-fmt
            fourmolu;
        };
        packages.default = self'.packages.project-haskell-template;
        devShells.default =
          let
            mkCommand = name: v:
              (v.package or pkgs.writeShellApplication { inherit name; text = v.command; }).overrideAttrs (oa: {
                meta.description = if v ? description then v.description else oa.meta.description or "No description";
              });
            wrapCommands = spec:
              let commands = lib.mapAttrsToList mkCommand spec;
              in
              pkgs.writeShellApplication {
                name = "dev";
                runtimeInputs = commands;
                text = ''
                  showHelp () {
                    echo "Available commands:"
                    echo
                    echo "  ${
                        lib.concatStringsSep "\n  " 
                          (map (drv: 
                            let name = builtins.baseNameOf (lib.getExe drv);
                                desc = drv.meta.description;
                            in name + " : " + desc
                          ) commands)
                    }"
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
          config.devShells.project.overrideAttrs (oa: {
            shellHook = (oa.shellHook or "") + ''
          '';
            nativeBuildInputs = (oa.nativeBuildInputs or [ ]) ++ [
              (
                (wrapCommands {
                  # TODO: banner-grouping 
                  hoog = {
                    description = "Start Hoogle server for project dependencies";
                    command = ''
                      echo http://127.0.0.1:8888
                      hoogle serve -p 8888 --local
                    '';
                    category = "Dev Tools";
                  };
                  repl = {
                    description = "Start the cabal repl";
                    command = ''
                      cabal repl
                    '';
                    category = "Dev Tools";
                  };
                  run = {
                    description = "Run the project with ghcid auto-recompile";
                    command = ''
                      ghcid -c "cabal repl exe:haskell-template" --warnings -T :main
                    '';
                    category = "Primary";
                  };
                }).overrideAttrs (_oa: {
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
}



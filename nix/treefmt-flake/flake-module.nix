{ self, inputs, lib, flake-parts-lib, ... }:
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
      ({ config, self', inputs', pkgs, system, ... }: {
        options.treefmt = mkOption {
          description = ''
            treefmt flake module options
          '';
          type = types.submodule {
            options = {
              config = mkOption {
                description = "treefmt-nix configuration";
                type = types.raw; # TODO: Should use module-options.nix
              };
              module = mkOption {
                type = types.raw; # TODO: module
                default =
                  inputs.treefmt-nix.lib.evalModule pkgs config.treefmt.config;
              };
              programs = mkOption {
                type = types.attrsOf types.package;
                default =
                  pkgs.lib.concatMapAttrs
                    (k: v:
                      if v.enable
                      then { "${k}" = v.package; }
                      else { })
                    config.treefmt.module.config.programs;
              };
              wrapper = mkOption {
                type = types.package;
                default = config.treefmt.module.config.build.wrapper;
              };
            };
          };
        };
      });
  };
  config = {
    perSystem = { config, self', inputs', pkgs, ... }: {
      checks.treefmt = pkgs.runCommandLocal "treefmt-check"
        {
          buildInputs = [ pkgs.git config.treefmt.wrapper ] ++ config.treefmt.programs;
        }
        ''
          set -e
          # treefmt uses a cache at $HOME. But we can use --no-cache
          # to make treefmt not use a cache. We still seem to need
          # to export a writable $HOME though.
          # TODO: https://github.com/numtide/treefmt/pull/174 fixes this issue
          # but we need to wait until a release is made and that release gets
          # into the nixpkgs we use.
          export HOME="$TMP"
          # `treefmt --fail-on-change` is broken for purs-tidy; So we must rely
          # on git to detect changes. An unintended advantage of this approach
          # is that when the check fails, it will print a helpful diff at the end.
          cp -r ${self} $HOME/project
          chmod -R a+w $HOME/project
          cd $HOME/project
          git init
          git config user.email "nix@localhost"
          git config user.name Nix
          git add .
          git commit -m init
          treefmt --no-cache
          git status
          git --no-pager diff --exit-code
          touch $out
        '';
    };
  };
}

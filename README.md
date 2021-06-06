# haskell-template

Haskell project template optimized for a fully reproducible and friendly development environment. Based on [Nix](https://notes.srid.ca/haskell-nix) + [Flakes](https://serokell.io/blog/practical-nix-flakes) + VSCode ([HLS](https://github.com/haskell/haskell-language-server)) + [ormolu](https://github.com/tweag/ormolu) autoformatting + [Relude](https://github.com/kowainik/relude#relude) as Prelude.

## Getting Started

- [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes)
- Run `nix-shell --run haskell-language-server` to sanity check your environment 
- [Open as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - Install the [workspace recommended](https://code.visualstudio.com/docs/editor/extension-marketplace#_workspace-recommended-extensions) extensions
    - <kbd>Ctrl+Shift+P</kbd> to run command "Nix-Env: Select Environment" and select `shell.nix`. The extension will ask you to reload VSCode at the end.
- Press <kbd>Ctrl+Shift+B</kbd> in VSCode, or run `bin/run` (`bin/run-via-tmux` if you have tmux installed) in terminal, to launch Ghcid running your program.

All but the final step need to be done only once.

Then, before using it for real,

- Rename all occurrences of `haskell-template` to `myproject`, as well as rename the cabal file to `myproject.cabal`. 
- Run `git add . && git commit -m rename` followed by `nix develop` (or `bin/run`) to verify that everything continues to work.

## Other templates

Some related templates include,

- [Serokell's Flake template](https://github.com/serokell/templates/tree/master/haskell-cabal2nix)
  - [Same, but using haskell.nix](https://github.com/serokell/templates/pull/2)

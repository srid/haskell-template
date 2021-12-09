# haskell-template

Haskell project template optimized for a fully reproducible and friendly development environment. Based on [Nix](http://www.srid.ca/haskell-nix) + [Flakes](https://serokell.io/blog/practical-nix-flakes) + VSCode ([HLS](https://github.com/haskell/haskell-language-server)) + [ormolu](https://github.com/tweag/ormolu) autoformatting + [Relude](https://github.com/kowainik/relude#relude) as Prelude.

## Getting Started

First-time setup:

- [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes)
- Run `nix-shell --run haskell-language-server` to sanity check your environment 
- [Open as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - When prompted by VSCode, install the [workspace recommended](https://code.visualstudio.com/docs/editor/extension-marketplace#_workspace-recommended-extensions) extensions
    - <kbd>Ctrl+Shift+P</kbd> to run command "Nix-Env: Select Environment" and then select `shell.nix`. 
        - The extension will ask you to reload VSCode at the end. Do it.

To run the program with auto-recompile:

- Press <kbd>Ctrl+Shift+B</kbd> in VSCode, or run `bin/run` (`bin/run-via-tmux` if you have tmux installed) in terminal, to launch Ghcid running your program.

Open `Main.hs`, and expect all HLS IDE features like hover-over tooltip to work out of the box. Try changing the source, and expect Ghcid to re-compile and re-run the app in the terminal below.

---

Renaming the project:

```sh
NAME=myproject
git mv haskell-template.cabal ${NAME}.cabal
nix run nixpkgs#sd -- haskell-template ${NAME} * */*
git add . && git commit -m rename
cd .. && mv haskell-template ${NAME} && cd ${NAME}
```

## Tips

- Run `nix flake update` to nixpkgs and other flake inputs.

## Alternatives

- haskell.nix: [Getting started with Flakes](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started-flakes.html)
- [Serokell's Flake template](https://github.com/serokell/templates/tree/master/haskell-cabal2nix)
  - [Same, but using haskell.nix](https://github.com/serokell/templates/pull/2)

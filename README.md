# haskell-template

Get a Haskell development environment up and running quickly. Thanks to Nix, this template is optimized for a fully reproducible and friendly development environment. It is based on:

- [Nix](https://srid.ca/haskell-nix) + [Flakes](https://serokell.io/blog/practical-nix-flakes) (via [`github:srid/haskell-flake`](https://github.com/srid/haskell-flake)) + GHC 9.6
- VSCode + [HLS](https://github.com/haskell/haskell-language-server)
- [fourmolu](https://github.com/fourmolu/fourmolu) autoformatting
- [Relude](https://github.com/kowainik/relude) as Prelude.
  - `.hlint.yaml` is [from relude](https://github.com/kowainik/relude/blob/main/.hlint.yaml)
- Devshell commands are provided via [just](https://just.systems/); run `just` in devshell.

If you have an *existing* Haskell project, you should probably use https://github.com/srid/haskell-flake instead.

## Getting Started

Initialize this template using:

```sh
nix run nixpkgs#omnix -- \
  init github:srid/haskell-template -o ./yourproject
```

*tldr: [Install Nix](https://nixos.asia/en/install), [setup direnv](https://nixos.asia/en/direnv), open in VSCode, install recommended extensions and run `just run`.*

Full instructions: https://srid.ca/haskell-template/start

Recommended dev environment setup: https://nixos.asia/en/direnv

## Tips

- Run `nix flake update` to update all flake inputs.
- Run `nix run nixpkgs#omnix ci` to build _all_ outputs.
- [pre-commit] hooks will automatically be setup in Nix shell. You can also run `pre-commit run -a` manually to run the hooks (e.g.: to autoformat the project tree using fourmolu, nixpkgs-fmt, etc. as well run programs like hlint). The hooks will checked as part of flake checks (thus CI).
- Run `just docs` to start Hoogle with packages in your cabal file.
- Run the application without installing: `nix run github:srid/haskell-template` (or `nix run .` from checkout)
- Common workflows
  - Adding library dependencies in Nix: https://community.flake.parts/haskell-flake/dependency
  - Adding tests: https://srid.ca/haskell-template/tests

## Discussions

Questions? Ideas? Suggestions? Join our [NixOS Asia community](https://nixos.asia/en/#community) or post in [Github Discussions](https://github.com/srid/haskell-template/discussions).

[pre-commit]: https://github.com/cachix/git-hooks.nix

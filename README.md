# haskell-template

Get a Haskell development environment up and running quickly. Thanks to Nix, this template is optimized for a fully reproducible and friendly development environment. It is based on:

- [Nix](https://srid.ca/haskell-nix) + [Flakes](https://serokell.io/blog/practical-nix-flakes) (via [`github:srid/haskell-flake`](https://github.com/srid/haskell-flake)) + GHC 9.4
- VSCode + [HLS](https://github.com/haskell/haskell-language-server)
- [fourmolu](https://github.com/fourmolu/fourmolu) autoformatting 
- [Relude](https://github.com/kowainik/relude) as Prelude.
  - `.hlint.yaml` is [from relude](https://github.com/kowainik/relude/blob/main/.hlint.yaml)
- Devshell commands are provided via [just](https://just.systems/); run `just` in devshell.

If you have an *existing* Haskell project, you should probably use https://github.com/srid/haskell-flake instead.

## Getting Started

*tldr: [Install Nix](https://nixos.asia/en/install), [setup direnv](https://nixos.asia/en/direnv), open in VSCode, install recommended extensions and run `just run`.*

Full instructions: https://srid.ca/haskell-template/start

Recommended dev environment setup: https://nixos.asia/en/direnv

## Tips

- Run `nix flake update` to update all flake inputs.
- Run `nix run github:srid/nixci build` to build _all_ outputs.
- Run `just fmt` in nix shell to autoformat the project. This uses [treefmt](https://github.com/numtide/treefmt).
- Run `just docs` to start Hoogle with packages in your cabal file.
- Run the application without installing: `nix run github:srid/haskell-template` (or `nix run .` from checkout)
- Common workflows
  - Adding library dependencies in Nix: https://community.flake.parts/haskell-flake/dependency
  - Adding tests: https://srid.ca/haskell-template/tests

## Discussions

Questions? Ideas? Suggestions? Join our [NixOS Zulip](https://nixos.zulipchat.com/#narrow/stream/413949-haskell-flake) or post in [Github Discussions](https://github.com/srid/haskell-template/discussions).

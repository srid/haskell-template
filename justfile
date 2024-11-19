default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ ARGS }}

# Autoformat the project tree
fmt:
    pre-commit run --all-files

# Run ghcid -- auto-recompile and run `main` function
run:
    ghcid -T :main

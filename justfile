default:
    @just --list

docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

repl *ARGS:
    cabal repl {{ARGS}}

fmt:
    treefmt

run:
    ghcid -c "cabal repl exe:haskell-template" --warnings -T :main

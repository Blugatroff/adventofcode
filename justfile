default:
    just run

build:
    stack build

run *ARGS:
    stack run {{ARGS}}


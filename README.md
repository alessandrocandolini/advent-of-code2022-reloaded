[![Compile and run tests](https://github.com/alessandrocandolini/advent-of-code2022-reloaded/actions/workflows/ci.yml/badge.svg)](https://github.com/alessandrocandolini/advent-of-code2022-reloaded/actions/workflows/ci.yml) [![codecov](https://codecov.io/gh/alessandrocandolini/advent-of-code2022-reloaded/graph/badge.svg?token=YHR9lK88dg)](https://codecov.io/gh/alessandrocandolini/advent-of-code2022-reloaded)

# advent-of-code2022-reloaded

The AoC 2022 solutions I wrote at that time can be found here:  
https://github.com/alessandrocandolini/advent-of-code2022

Since then, I’ve learned a thing or two. Time to solve them again in fresh new ways.

## How to build and run locally

The project uses the [Haskell tool stack](https://docs.haskellstack.org/en/stable/README/). The recommended way to install stack is by using [ghcup](https://www.haskell.org/ghcup/).
It's also possible to use [the nix package manager](https://nixos.org/), but this project does not provide a nix configuration file.

Assuming `stack` is installed in the system, to **build** the project use
```
stack build
```
To **build and run the tests**, run
```
stack test
```
or equivalently
```
stack build --test
```
For **faster feedback loop** during development, it's possible to run tests continuously on every file change:
```
stack test --fast --file-watch
```
To run tests with **test coverage** instrumentation,
```
stack test --coverage
```
which generates a textual and HTML report. Tests are run in the CI and test coverage reports are automatically uploaded to codecov.

To **run the executable via stack**,
```
stack exec aoc2022
```
or passing arguments
```
stack exec aoc2022 -- solve -d <day> -f <filename>
```

To copy the executable to the project root and run it as `./aoc2022`,
```
stack build --copy-bins --local-bin-path .
./aoc2022
```

To **install** the executable under `~/.local/bin`,
```
stack install
```
and the executable can be run with `aoc2022` or passing arguments like
```
aoc2022 solve -d 1 -f input
```
assuming `~/.local/bin` is in the `$PATH` variable.

To get more insights into runtime performance, it's possible to extract information from the Haskell RTS:
```
aoc2022 solve -d <day> -f <filename> +RTS -s -N1
```

For example, for day 1:
```
aoc2022 solve -d 1 -f input +RTS -s -N1
```

From standard input:
```
aoc2022 solve -d 1 --with-input +RTS -s -N1 < input
```

From `pbpaste` on macOS:
```
aoc2022 solve -d 1 --with-input +RTS -s -N1 < <(pbpaste)
```

To run a version of **ghci** compatible with the resolver
```
stack ghci
```
For more information about how to use `stack`, refer to the [official docs](https://docs.haskellstack.org/en/stable/).

## Available commands

Thanks to [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative), the CLI automatically generates documentation from code. It's recommended to use the generated helper to explore all the options. However, a summary is provided here of the most relevant options.

### Solve puzzles

From **file**:
```
aoc2022 solve -d 1 -f input
```

From **standard input**:
```
aoc2022 solve -d 1 --with-input < input
```
or
```
cat input | aoc2022 solve -d 1 --with-input
```

From **pbpaste** on macOS:
```
aoc2022 solve -d 1 --with-input < <(pbpaste)
```

### Retrieve stats
```
export AOC_SESSION=<insert the cookie value>
aoc2022 stats
aoc2022 stats -y 2022
aoc2022 stats -y 2022 --json
```

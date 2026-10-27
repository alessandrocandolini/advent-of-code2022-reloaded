[![Compile and run tests](https://github.com/alessandrocandolini/advent-of-code2022-reloaded/actions/workflows/ci.yml/badge.svg)](https://github.com/alessandrocandolini/advent-of-code2022-reloaded/actions/workflows/ci.yml) [![codecov](https://codecov.io/gh/alessandrocandolini/advent-of-code2022-reloaded/graph/badge.svg?token=YHR9lK88dg)](https://codecov.io/gh/alessandrocandolini/advent-of-code2022-reloaded)

# advent-of-code2022-reloaded

The AoC 2022 solutions I wrote at that time can be found here:  
https://github.com/alessandrocandolini/advent-of-code2022

Since then, I’ve learned a thing or two. Time to solve them again in fresh new ways.

## How to build and run locally

The project uses [Cabal](https://cabal.readthedocs.io/) with GHC **9.10.3**.
Install GHC and cabal-install using [GHCup](https://www.haskell.org/ghcup/).
CI uses cabal-install **3.18.1.0**.

`advent-of-code2022-reloaded.cabal` is the package definition. Edit it directly
when adding dependencies or modules. For example, after creating
`src/Example.hs`, add `Example` to the library's `exposed-modules`.
List new test modules in the test suite's `other-modules`; `hspec-discover`
continues to discover their specs.

`cabal.project` selects the compiler and imports the LTS 24.63 dependency
constraints used before the migration. `cabal.project.freeze` records the resolved
versions, flags, and Hackage index state. After intentionally changing dependencies,
update the constraints as needed, regenerate the freeze file with `cabal freeze`,
and run the tests.

Update the package index, then build and test:

```bash
cabal update
cabal build all
cabal test all --test-show-details=direct
```

For faster development builds:

```bash
cabal test all --disable-optimization --test-show-details=direct
```

To run tests with coverage instrumentation:

```bash
cabal test all --enable-coverage --test-show-details=direct
```

Cabal prints the location of the HTML coverage report under `dist-newstyle/`.
CI builds all components, runs the tests with coverage, and uploads the results
to Codecov.

Run the executable, optionally passing arguments after `--`:

```bash
cabal run aoc2022 -- --help
cabal run aoc2022 -- solve -d 1 -f input
```

To install the executable in the project root:

```bash
cabal install exe:aoc2022 --installdir=. --install-method=copy --overwrite-policy=always
./aoc2022 --help
```

To install it under `~/.local/bin`:

```bash
cabal install exe:aoc2022 --installdir="$HOME/.local/bin" --install-method=copy --overwrite-policy=always
```

With that directory on `PATH`, run `aoc2022` directly. Runtime statistics can be
requested with `+RTS -s -N1`, for example:

```bash
aoc2022 solve -d 1 -f input +RTS -s -N1
```

To start GHCi with the library:

```bash
cabal repl lib:advent-of-code2022-reloaded
```

### HLS

`hie.yaml` uses a Cabal cradle with `component: all`, and `cabal.project` enables
tests. This lets HLS load the library, executable, and test suite together;
multi-component loading requires Cabal 3.12 or newer.

The project's `hls.json` enables multi-component loading and cross-module rename
for **HLS 2.15**. The `haskell-tools.nvim` plugin reads this file automatically.
For other clients, set `haskell.componentsLoading = "multi: whole-project"` in
their HLS settings. On HLS 2.14, use `"sessionLoading": "multipleComponents"`
instead. The project also requires Cabal library 3.16 or newer for custom
dependency setup scripts so HLS can use `--with-repl`.

Restart HLS after switching build tools and wait for indexing to finish.
Cross-module rename is experimental; inspect the diff and run `cabal test all`
afterward. See the [HLS rename documentation](https://haskell-language-server.readthedocs.io/en/2.14.0.0/features.html#rename).

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

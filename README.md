# Advent of Code 2024

Solutions for ❄️[Advent of Code 2024]❄️ in [Haskell]

| Su   | Mo   | Tu   | We   | Th   | Fr   | Sa   |
| ---- | ---- | ---- | ---- | ---- | ---- | ---- |
| 1 ⭐ | 2 ⭐ | 3 ⭐ | 4 ⭐ | 5 ⭐ | 6 ⭐ | 7 ⭐ |
| 8 ⭐ | 9 ⭐ | 10⭐ | 11⭐ | 12⭐ | 13   | 14   |
| 15   | 16   | 17   | 18   | 19   | 20   | 21   |
| 22   | 23   | 24   | 25   | 26   | 27   | 28   |
| 29   | 30   | 31   |      |      |      |      |

## Help

### Using stack directly

This project uses the [`hspec`][hspec] test runner as its entry point,
so the solutions are written as tests. Pass `--help` to the executable
with `stack run` or `stack run advent2024` to see options available for
running and filtering tests:

```console
$ stack run -- --help

Usage: advent2024 [OPTION]...

OPTIONS
              --help              display this help and exit
              --ignore-dot-hspec  do not read options from ~/.hspec and .hspec
  -m PATTERN  --match=PATTERN     only run examples that match given PATTERN
              --skip=PATTERN      skip examples that match given PATTERN

RUNNER OPTIONS
        --[no-]dry-run          pretend that everything passed; don't verify
                                anything
...etc.
```

### Using make

Alternatively, use `make solve` to run all solutions or
`make run day=N part=N` to run a specific solution. Run `make` without
a target to see the other available targets:

```console
$ make
Usage
solve                 Test all solutions
run                   Test a solution for one day by setting [day=N] [part=N]
opt                   Build optimized
build                 Build unoptimized
update                Update compiler, dependencies, and tools
update.stack          Set up the compiler and project dependencies
update.tools          Install additional tooling
clean                 Clean project
```

### Stubbing out new modules

Use the `stub.py` script to stub out a new module and download the
day's puzzle input:

```console
$ ./stub.py -h
usage: stub.py [-h] [-f] DAY [DAY ...]

stub out Day{DAY}Spec.hs modules and empty input files

positional arguments:
  DAY          day within the closed interval [1..25]

optional arguments:
  -h, --help   show this help message and exit
  -f, --force  overwrite extant modules
```

Note that by default the script will avoid overwriting extant modules:

```console
$ ./stub.py 1 2
usage: stub.py [-h] [-f] DAY [DAY ...]
stub.py: error: cowardly refusing to overwrite src/Advent/Day01Spec.hs, src/Advent/Day02Spec.hs
```

Override this behavior with `-f`/`--force`:

```console
$ ./stub.py 1 2 --force
force overwriting src/Advent/Day01Spec.hs, src/Advent/Day02Spec.hs

$ git status
On branch main

Changes not staged for commit:
    modified:   src/Advent/Day01Spec.hs
    modified:   src/Advent/Day02Spec.hs
```

Note that, [per the AoC creator's wishes][Copying], puzzle input is not
kept in version control:

> Advent of Code is free to use, not free to copy. If you're posting a
> code repository somewhere, please don't include parts of Advent of
> Code like the puzzle text or your inputs

[Advent of Code 2024]: https://adventofcode.com/2024
[Haskell]: https://www.haskell.org
[hspec]: https://hspec.github.io
[Copying]: https://adventofcode.com/about#faq_copying

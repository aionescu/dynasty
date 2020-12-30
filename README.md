# dynasty

Dynasty is an interpreted, dynamically-typed, lazily evaluated, purely functional programming language.

*:construction: Dynasty is still very early in development. Expect things to change and/or break often. :construction:*

## Examples

You can find some test programs in the [Examples](Examples) folder.

The most interesting one is the [Brainfuck interpreter](Examples/bf.dy).

## TODO

Some major features are not yet implemented:

* Whitespace-sensitive parser
* Module system & import mechanism
* REPL

## Building & running

To build the project, you will need `cabal`, which can be found [here](https://www.haskell.org/platform/).

To build, run `cabal new-build` in the root of the repo.

You can then either install the package globally running `cabal install`, or run the local build with `cabal new-run` (e.g. `cabal new-run . -- Examples/fac.dy`).

## License

This repository is licensed under the terms of the GNU General Public License v3.
For more details, see [the license file](LICENSE.txt).

<div align="center">
  <h1>dynasty</h1>

  <img src="assets/logo.png" alt="Dynasty Logo" width="150" height="150"/>
</div>

Dynasty is an interpreted, dynamically-typed, lazily evaluated, purely functional programming language.

## Examples

You can find some test programs in the [examples](examples) folder.

The most interesting one is the [Brainfuck interpreter](examples/bf/bf.dy).

## Building & running

### Prerequisites

* GHC (>= 9.0.2)
* cabal (>= 3.6)

(Can be installed via [ghcup](https://www.haskell.org/ghcup/))

### Build the interpreter

```sh
cabal build
```

### Run the interpreter

```sh
cabal run . <script file> <script args>
```

If not already built, `cabal run` will also build the project.

e.g.

```sh
cabal run . examples/fac.dy
```

## License

This repository is licensed under the terms of the GNU General Public License v3.
For more details, see [the license file](LICENSE.txt).

<div align="center">
  <h1>dynasty</h1>

  <img src="assets/logo.png" alt="Dynasty Logo" width="150" height="150"/>
</div>

Dynasty is a dynamically-typed, lazily evaluated, purely functional programming language that compiles to JavaScript.

## Examples

You can find some Dynasty programs in the [examples](examples) folder, and the standard library (called `Core`) lives in the [core](core) folder.

## Building & running the compiler

### Prerequisites

* GHC (>= 9.0)
* cabal (>= 3.6)

(Can be installed via [ghcup](https://www.haskell.org/ghcup/))

To compile Dynasty programs, use the following command:

```sh
cabal run . -- <target-dir>
```

The first invocation may take a while, since it will also build the compiler's dependencies.

To run the resulting JavaScript, you can include it in a `<script>` tag inside a HTML document to load it in the browser, or run it with [Node.js](https://nodejs.org/en/) or [Deno](https://deno.land/).

## VS Code Extension

To install the VS Code extension, simply run the `install-vscode-ext.sh` script.

## License

This repository is licensed under the terms of the GNU General Public License v3.
For more details, see [the license file](LICENSE.txt).

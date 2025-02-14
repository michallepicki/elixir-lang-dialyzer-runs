# Daily Dialyzer checks on Elixir source code

[![Dialyzer](https://github.com/michallepicki/elixir-lang-dialyzer-runs/workflows/dialyzer/badge.svg?branch=master)](https://github.com/michallepicki/elixir-lang-dialyzer-runs/actions?query=workflow%3Adialyzer+branch%3Amaster)

This repository runs a [Dialyzer](http://erlang.org/doc/man/dialyzer.html) check on the [Elixir programming language source code](https://github.com/elixir-lang/elixir) to

> identify software discrepancies, such as definite type errors, code that has become dead or unreachable because of programming error

in the Elixir source code itself. Elixir CI only checks that types and specs syntax generated by the compiler is valid (without actually performing Dialyzer analysis), and there is a small [Dialyzer test suite](https://github.com/elixir-lang/elixir/tree/main/lib/elixir/test/elixir/fixtures/dialyzer) to ensure core macros don't cause Dialyzer issues.

Unfortunately this project won't find all issues that propagate and show up in Elixir stdlib or tooling code _usage_ (e.g. warnings in your correct Elixir code caused by wrong specs in the Elixir stdlib) because this project doesn't analyze tests where most of stdlib usage lives - Elixir tests are interpreted and don't produce `.beam` files that can be analyzed with Dialyzer easily. 

Reports can be found at https://github.com/michallepicki/elixir-lang-dialyzer-runs/actions?query=workflow%3Adialyzer+branch%3Amaster . Click the last event, then `dialyzer` on the left, then expand "Check Elixir" step to see issues. The report is also uploaded as a build artifact. Potential issues are at the end of the output (if any). Filtered non-issues are printed but don't affect the success/failure status code.

Occasionally, filtering code will have to be updated when line numbers change.

## How to run the check locally

```
# clone the repositories:
git clone https://github.com/michallepicki/elixir-lang-dialyzer-runs.git
cd elixir-lang-dialyzer-runs
git clone https://github.com/elixir-lang/elixir.git --depth=1

# build elixir:
cd elixir
make clean compile
cd ../

# prepare the base Erlang PLT and run analysis on Elixir source code:
elixir/bin/elixir dialyzer.exs
```

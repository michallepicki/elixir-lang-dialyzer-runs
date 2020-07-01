# Daily dialyzer checks on Elixir source code

[![dialyzer](https://github.com/michallepicki/elixir-lang-dialyzer-runs/workflows/dialyzer/badge.svg?branch=master)](https://github.com/michallepicki/elixir-lang-dialyzer-runs/actions?query=workflow%3Adialyzer+branch%3Amaster)

This repository runs a dialyzer check on the [Elixir programming language source code](https://github.com/elixir-lang/elixir).

Reports can be found at https://github.com/michallepicki/elixir-lang-dialyzer-runs/actions?query=workflow%3Adialyzer+branch%3Amaster . Click the last event, then `dialyze` on the left, then expand "Run dialyzer" step to see errors. The report is also uploaded as a build artifact. Potential issues are at the top of the output (if any). Filtered non-issues are printed but don't affect the success/failure status code.

Occasionally, filtering code will have to be updated when line numbers change.

## How to run the check locally

```
git clone git@github.com:michallepicki/elixir-lang-dialyzer-runs.git
cd elixir-lang-dialyzer-runs
git clone git@github.com:elixir-lang/elixir.git
cd elixir
make clean compile build_plt
mv .elixir.plt ../plt-dir/
cd ../
elixir/bin/elixir dialyze.exs
```
# Daily dialyzer checks on Elixir source code

![dialyzer](https://github.com/michallepicki/elixir-lang-dialyzer-runs/workflows/dialyzer/badge.svg?branch=master&event=schedule)

This repository runs a dialyzer check on the Elixir source code.

Reports can be found at https://github.com/michallepicki/elixir-lang-dialyzer-runs/actions . Click the last event, then `dialyze` on the left, then expand "Run dialyzer" step to see errors. The report is also uploaded as a build artifact. Potential issues are at the top of the output (if any). Filtered non-issues are printed but don't affect the success/failure status code.

Occasionally, filtering code will have to be updated when line numbers change.
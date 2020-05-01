report = :dialyzer.run(
  init_plt: 'plt-dir/.elixir.plt',
  include_dirs: ['elixir/lib/elixir/ebin'],
  native: true,
  warnings: [
    :unknown
  ],
  files_rec: [
    'elixir/lib/eex/ebin',
    'elixir/lib/elixir/ebin'
    'elixir/lib/ex_unit/ebin',
    'elixir/lib/iex/ebin',
    'elixir/lib/logger/ebin',
    'elixir/lib/mix/ebin'
  ]
)
|> Enum.map(fn warning ->
  warning
  |> :dialyzer.format_warning(indent_opt: true)
  |> to_string()
end)
|> Enum.join("\n")

File.write("report", report)

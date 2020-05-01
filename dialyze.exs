report =
  :dialyzer.run(
    init_plt: 'plt-dir/.elixir.plt',
    include_dirs: ['elixir/lib/elixir/ebin'],
    native: true,
    warnings: [
      :unknown
    ],
    files_rec: [
      'elixir/lib/eex/ebin',
      'elixir/lib/elixir/ebin',
      'elixir/lib/ex_unit/ebin',
      'elixir/lib/iex/ebin',
      'elixir/lib/logger/ebin',
      'elixir/lib/mix/ebin'
    ]
  )
  |> Stream.map(fn warning ->
    warning
    |> :dialyzer.format_warning(indent_opt: true)
    |> (to_string() <>
          "\n")
  end)
  |> Stream.into(File.stream!("report", [:write, :utf8]))
  |> Stream.into(IO.stream(:stdio, :line))
  |> Stream.run()

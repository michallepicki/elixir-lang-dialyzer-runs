defmodule Dialyzin do
  def filter([], acc) do
    acc
  end

  def filter([warning | rest], acc) do
    filter(rest, [filter_warning(warning) | acc])
  end

  defp filter_warning(warning = {:warn_failing_call, {'lib/logger.ex', line}, {:call, [:logger, :macro_log, [?(, ?\#, ?{, ?}, ?, | _], [1], :only_contract, _, _, _]}}) when line in [905, 896],
    do: {:ok, "Elixir deliberately using erlang macro-based logger interface without passing in call location", warning}

  defp filter_warning(
         warning =
           {:warn_opaque, {'lib/mix/tasks/test.ex', 559},
            {:opaque_match,
             [
               'pattern \#{\'__struct__\':=\'Elixir.MapSet\'}',
               '\'Elixir.MapSet\':t(binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | char(),binary() | []))',
               '\'Elixir.MapSet\':t(binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | char(),binary() | []))'
             ]}}
       ),
       do: {:ok, "Elixir folks want to be able to pattern match on a struct name while keeping the struct type opaque", warning}

  defp filter_warning(warning = {:warn_matching, {'lib/kernel.ex', line}, {:pattern_match, ['pattern \'false\'', '\'true\'']}}) when line in [1798, 3001, 3325, 3630, 3975, 3979, 4416],
    do: {:ok, "inlined bootstrap check stuff", warning}

  defp filter_warning(warning = {:warn_matching, {'src/elixir_erl_compiler.erl', 59}, {:pattern_match, _lots_of_details}}),
    do: {:ok, "return type not documented in erlang", warning}

  defp filter_warning(warning = {:warn_matching, {'lib/mix/tasks/compile.erlang.ex', 104}, {:pattern_match, ['pattern {\'error\', \'badarg\'}', _]}}),
    do: {:ok, "return type not documented in erlang", warning}

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/mix/tasks/test.ex', 404}, {:no_return, [:only_normal, :raise_with_shell, 2]}}),
    do: {:ok, "not annotated exception", warning}

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/mix/release.ex', 227}, {:no_return, [:only_normal, :bad_umbrella!, 0]}}),
    do: {:ok, "not annotated exception", warning}

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/mix/dep/loader.ex', 259}, {:no_return, [:only_normal, :invalid_dep_format, 1]}}),
    do: {:ok, "not annotated exception", warning}

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/iex.ex', 652}, {:no_return, [:only_normal, :__break__!, 2]}}),
    do: {:ok, "not annotated exception", warning}

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/iex/cli.ex', 151}, {:no_return, [:only_normal]}}),
    do: {:ok, "not annotated exit", warning}

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/elixir/src/elixir_parser.yrl', _}, {:no_return, [:only_normal, function, _arity]}})
       when function in [
              :error_invalid_kw_identifier,
              :error_no_parens_container_strict,
              :error_no_parens_many_strict,
              :error_no_parens_strict,
              :error_bad_atom,
              :error_invalid_stab,
              :return_error,
              :yeccpars2_300_,
              :yeccpars2_289_,
              :yeccpars2_88_,
              :yeccpars2_357_,
              :yeccpars2_356_,
              :yeccpars2_320_,
              :yeccpars2_86_
            ],
       do: {:ok, "parser not annotated exception", warning}

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/elixir/src/elixir_parser.erl', _}, {:no_return, [:only_normal, function, _arity]}})
       when function in [
              :yeccpars2_357,
              :yeccpars2_356,
              :yeccpars2_320,
              :yeccpars2_300,
              :yeccpars2_88,
              :yeccpars2_86
            ],
       do: {:ok, "parser not annotated exception", warning}

  defp filter_warning(warning = {:warn_unknown, {[], 0}, {:unknown_function, {module, :__impl__, 1}}})
       when module in [
              Collectable.Atom,
              Collectable.Float,
              Collectable.Function,
              Collectable.Integer,
              Collectable.PID,
              Collectable.Port,
              Collectable.Reference,
              Collectable.Tuple,
              Enumerable.Atom,
              Enumerable.BitString,
              Enumerable.Float,
              Enumerable.Integer,
              Enumerable.PID,
              Enumerable.Port,
              Enumerable.Reference,
              Enumerable.Tuple,
              List.Chars.Function,
              List.Chars.Map,
              List.Chars.PID,
              List.Chars.Port,
              List.Chars.Reference,
              List.Chars.Tuple,
              String.Chars.Function,
              String.Chars.Map,
              String.Chars.PID,
              String.Chars.Port,
              String.Chars.Reference,
              String.Chars.Tuple
            ],
       do: {:ok, "some protocol consolidation stuff", warning}

  defp filter_warning(warning = {:warn_unknown, {[], 0}, {:unknown_function, {Hex, function, 0}}}) when function in [:start, :version],
    do: {:ok, "Hex package loading gets handled by the Mix task", warning}

  defp filter_warning(warning = {:warn_not_called, {'lib/base.ex', _}, {:unused_fun, [function, _]}}) when function in [:encode_pair_clauses, :shift, :encode_clauses, :decode_char_clauses, :decode_mixed_clauses, :decode_clauses, :bad_digit_clause],
    do: {:ok, "functions inlined or only used to generate other functions at compile time", warning}

  defp filter_warning(warning), do: {:warning, warning}
end

warnings =
  :dialyzer.run(
    init_plt: 'plt-dir/.elixir.plt',
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

warnings
|> Dialyzin.filter([])
|> Enum.sort_by(fn
  {:warning, _} -> 1
  {:ok, _, _} -> 2
end)
|> Enum.map(fn
  {:warning, warning} ->
    formatted_warning =
      warning
      |> :dialyzer.format_warning(indent_opt: true, filename_opt: :fullpath)
      |> to_string()

    # "Potential issue: #{formatted_warning}\n\n" <> "raw form: #{inspect(warning)}\n\n"
    formatted_warning <> "\n"

  {:ok, _message, _warning} ->
    # "...something we can ignore (#{message})\n\n" <> "raw form: #{inspect(warning)}\n\n"
    nil
end)
|> Enum.filter(& &1)
|> Stream.into(File.stream!("report", [:write, :utf8]))
|> Stream.into(IO.stream(:stdio, :line))
|> Stream.run()

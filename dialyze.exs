defmodule Dialyze do
  expected = %{}

  def filter(warnings) do
    filter(warnings, [])
  end

  defp filter([], acc) do
    Enum.reverse(acc)
  end

  defp filter([warning | rest], acc) do
    filter(rest, [filter_warning(warning) | acc])
  end

  defp filter_warning(warning = {:warn_failing_call, {'lib/logger.ex', 770}, {:call, [Logger, :__do_log__, _, [3], :only_sig, _, _, {false, :none}]}}),
    do: {:ok, "Elixir deliberately using erlang macro-based logger interface without passing in call location", warning, 1}

  expected = Map.put(expected, 1, 1)

  defp filter_warning(
         warning =
           {:warn_opaque, {'lib/mix/tasks/test.ex', 613},
            {:opaque_match,
             [
               'pattern \#{\'__struct__\':=\'Elixir.MapSet\'}',
               '\'Elixir.MapSet\':t(binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | char(),binary() | []))',
               '\'Elixir.MapSet\':t(binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | char(),binary() | []))'
             ]}}
       ),
       do: {:ok, "Elixir folks want to be able to pattern match on a struct name while keeping the struct type opaque", warning, 2}

  expected = Map.put(expected, 2, 1)

  defp filter_warning(warning = {:warn_matching, {'lib/kernel.ex', _}, {:pattern_match, ['pattern \'false\'', '\'true\'']}}),
    do: {:ok, "inlined bootstrap check stuff", warning, 3}

  expected = Map.put(expected, 3, 7)

  defp filter_warning(warning = {:warn_matching, {'src/elixir_erl_compiler.erl', 59}, {:pattern_match, _lots_of_details}}),
    do: {:ok, "return type not documented in erlang", warning, 4}

  expected = Map.put(expected, 4, 1)

  defp filter_warning(warning = {:warn_matching, {'lib/mix/tasks/compile.erlang.ex', 103}, {:pattern_match, ['pattern {\'error\', \'badarg\'}', _]}}),
    do: {:ok, "return type not documented in erlang", warning, 5}

  expected = Map.put(expected, 5, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/mix/tasks/test.ex', _}, {:no_return, [:only_normal, :raise_with_shell, 2]}}),
    do: {:ok, "not annotated exception", warning, 6}

  expected = Map.put(expected, 6, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/mix/release.ex', _}, {:no_return, [:only_normal, :bad_umbrella!, 0]}}),
    do: {:ok, "not annotated exception", warning, 7}

  expected = Map.put(expected, 7, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/mix/dep/loader.ex', _}, {:no_return, [:only_normal, :invalid_dep_format, 1]}}),
    do: {:ok, "not annotated exception", warning, 8}

  expected = Map.put(expected, 8, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/iex.ex', _}, {:no_return, [:only_normal, :__break__!, 2]}}),
    do: {:ok, "not annotated exception", warning, 9}

  expected = Map.put(expected, 9, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'src/elixir_quote.erl', _}, {:no_return, [:only_normal, :bad_escape, 1]}}),
    do: {:ok, "not annotated exception", warning, 10}

  expected = Map.put(expected, 10, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/mix/tasks/iex.ex', _}, {:no_return, [:only_normal, :run, 1]}}),
    do: {:ok, "not annotated exception", warning, 11}

  expected = Map.put(expected, 11, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/iex/cli.ex', _}, {:no_return, [:only_normal]}}),
    do: {:ok, "not annotated exit", warning, 12}

  expected = Map.put(expected, 12, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/gen_event.ex', _}, {:no_return, [:only_normal, :system_terminate, 4]}}),
    do: {:ok, "not annotated exit", warning, 13}

  expected = Map.put(expected, 13, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/ex_unit/signal_handler.ex', 26}, {:no_return, [:only_normal, :handle_event, 2]}}),
    do: {:ok, "not annotated exit", warning, 14}

  expected = Map.put(expected, 14, 1)

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
       do: {:ok, "parser not annotated exception", warning, 15}

  expected = Map.put(expected, 15, 14)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/elixir/src/elixir_parser.erl', _}, {:no_return, [:only_normal, function, _arity]}})
       when function in [
              :yeccpars2_357,
              :yeccpars2_356,
              :yeccpars2_320,
              :yeccpars2_300,
              :yeccpars2_88,
              :yeccpars2_86
            ],
       do: {:ok, "parser not annotated exception", warning, 16}

  expected = Map.put(expected, 16, 6)

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
       do: {:ok, "some protocol consolidation stuff", warning, 17}

  expected = Map.put(expected, 17, 28)

  defp filter_warning(warning = {:warn_unknown, {[], 0}, {:unknown_function, {Hex, function, 0}}}) when function in [:start, :version],
    do: {:ok, "Hex package loading gets handled by the Mix task", warning, 18}

  expected = Map.put(expected, 18, 2)

  defp filter_warning(warning = {:warn_not_called, {'lib/base.ex', _}, {:unused_fun, [function, _]}}) when function in [:encode_pair_clauses, :shift, :encode_clauses, :decode_char_clauses, :decode_mixed_clauses, :decode_clauses, :bad_digit_clause],
    do: {:ok, "functions inlined or only used to generate other functions at compile time", warning, 19}

  expected = Map.put(expected, 19, 7)

  defp filter_warning(warning = {:warn_not_called, {'lib/system.ex', _}, {:unused_fun, [function, 1]}}) when function in [:read_stripped, :strip],
    do: {:ok, "functions called only during elixir compilation time", warning, 20}

  expected = Map.put(expected, 20, 2)

  defp filter_warning(warning = {:warn_matching, {'lib/mix/dep/loader.ex', line}, {:pattern_match_cov, ['variable _', '\'Elixir.Regex\'']}}) when line in [125, 155, 163],
    do: {:ok, "redundant clause generated by is_struct macro for this literal atom", warning, 21}

  expected = Map.put(expected, 21, 3)

  defp filter_warning(warning = {:warn_matching, {'lib/ex_unit/assertions.ex', 741}, {:pattern_match_cov, ['variable _', '\'Elixir.Regex\'']}}),
    do: {:ok, "redundant clause generated by is_struct macro for this literal atom", warning, 22}

  expected = Map.put(expected, 22, 1)

  defp filter_warning(
         warning =
           {:warn_matching, {'lib/dynamic_supervisor.ex', 350},
            {:pattern_match_cov,
             [
               'variable _other@1',
               '{_,_,_,_,_,_} | \#{\'id\':=_, \'start\':={atom(),atom(),[any()]}, \'modules\'=>\'dynamic\' | [atom()], \'restart\'=>\'permanent\' | \'temporary\' | \'transient\', \'shutdown\'=>\'brutal_kill\' | \'infinity\' | non_neg_integer(), \'type\'=>\'supervisor\' | \'worker\'}'
             ]}}
       ),
       do: {:ok, "overly defensive code", warning, 23}

  expected = Map.put(expected, 23, 1)

  defp filter_warning(warning = {:warn_matching, {'lib/string_io.ex', 410}, {:guard_fail, [:is_list, '(_data@1::binary())']}}),
    do: {:ok, "overly defensive code", warning, 24}

  expected = Map.put(expected, 24, 1)

  defp filter_warning(
         warning =
           {:warn_matching, {'lib/mix/utils.ex', 694},
            {:pattern_match,
             [
               'pattern \'nil\'',
               '\#{\'__struct__\':=\'Elixir.URI\', \'authority\':=\'nil\' | binary(), \'fragment\':=\'nil\' | binary(), \'host\':=\'nil\' | binary(), \'path\':=\'nil\' | binary(), \'port\':=\'nil\' | char(), \'query\':=\'nil\' | binary(), \'scheme\':=\'nil\' | binary(), \'userinfo\':=\'nil\' | binary()}'
             ]}}
       ),
       do: {:ok, "overly defensive code", warning, 25}

  expected = Map.put(expected, 25, 1)

  defp filter_warning(warning = {:warn_matching, {'lib/iex/helpers.ex', 604}, {:pattern_match, ['pattern <__key@1, \'nil\'>', '<<<_:64,_:_*8>>,<<_:80>> | string() | non_neg_integer()>']}}),
    do: {:ok, "overly_defensive code", warning, 26}

  expected = Map.put(expected, 26, 1)

  defp filter_warning(warning = {:warn_matching, {'lib/mix/tasks/deps.compile.ex', 245}, {:guard_fail, ['_@6::\'true\'', '=:=', '\'nil\'']}}),
    do: {:ok, "slightly dead code", warning, 27}

  expected = Map.put(expected, 27, 1)

  defp filter_warning(
         warning =
           {:warn_matching, {'lib/calendar/time.ex', 642},
            {:pattern_match, ['pattern {\'error\', _reason@1}', '{\'ok\',\#{\'__struct__\':=\'Elixir.Time\', \'calendar\':=atom(), \'hour\':=non_neg_integer(), \'microsecond\':={non_neg_integer(),non_neg_integer()}, \'minute\':=non_neg_integer(), \'second\':=non_neg_integer()}}']}}
       ),
       do: {:ok, "slightly dead code", warning, 28}

  expected = Map.put(expected, 28, 1)

  defp filter_warning(warning), do: {:warning, warning}

  @expected_counts expected
  def expected_counts(), do: @expected_counts
end

results =
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
  |> Dialyze.filter()

filtered =
  results
  |> Enum.reduce(%{}, fn
    {:warning, _}, acc ->
      acc

    {:ok, message, warning, id}, acc ->
      Map.update(
        acc,
        id,
        %{count: 1, message: message, warnings: [warning]},
        fn map ->
          %{map | count: map.count + 1, warnings: [warning | map.warnings]}
        end
      )
  end)

results =
  (results ++
     (Dialyze.expected_counts()
      |> Enum.filter(fn
        {id, expected_count} ->
          filtered[id][:count] != expected_count
      end)
      |> Enum.map(fn
        {id, expected_count} ->
          {:unexpected_count, filtered[id][:message], filtered[id][:warnings], id, filtered[id][:count], expected_count}
      end)))
  |> Enum.sort_by(fn
    {:warning, _} -> 1
    {:unexpected_count, _, _, _, _, _} -> 2
    {:ok, _, _, _} -> 3
  end)

has_potential_issues? =
  case results do
    [{:warning, _} | _] -> true
    [{:unexpected_count, _, _, _, _, _} | _] -> true
    _ -> false
  end

results
|> Stream.map(fn
  {:warning, warning} ->
    formatted_warning =
      warning
      |> :dialyzer.format_warning(indent_opt: true, filename_opt: :fullpath)
      |> to_string()

    "############################################################\n" <>
      "Potential issue: \n#{formatted_warning}\n\n" <>
      "raw form: #{inspect(warning)}\n\n"

  {:unexpected_count, _message, _warnings, id, count, expected_count} ->
    "############################################################\n" <>
      "Expected count of filtered non-issues ##{id} is off! Expected #{expected_count}, found #{count || 0} issues\n\n"

  {:ok, message, warning, id} ->
    formatted_warning =
      warning
      |> :dialyzer.format_warning(indent_opt: true, filename_opt: :fullpath)
      |> to_string()

    "############################################################\n" <>
      "Filtered non-issue ##{id} (#{message}):\n#{formatted_warning}\n\n" <>
      "raw form: #{inspect(warning)}\n\n"
end)
|> Stream.into(File.stream!("report", [:write, :utf8]))
|> Stream.into(IO.stream(:stdio, :line))
|> Stream.run()

if(has_potential_issues?) do
  System.halt(1)
end

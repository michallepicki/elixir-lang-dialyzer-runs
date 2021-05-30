defmodule Dialyze do
  require Record

  Record.defrecord(:filtered, id: nil, comment: nil, data: nil)
  Record.defrecord(:unfiltered, data: nil)
  Record.defrecord(:unexpected_count, id: nil, actual: nil, expected: nil)

  expected_counts = %{}

  defp filter(warnings), do: filter(warnings, [])

  defp filter([], acc), do: Enum.reverse(acc)

  defp filter([warning | rest], acc), do: filter(rest, [filter_warning(warning) | acc])

  defp filter_warning(warning = {:warn_failing_call, {'lib/logger.ex', _}, {:call, [Logger, :__do_log__, _, [3], :only_sig, _, _, {false, :none}]}}),
    do: filtered(id: 1, comment: "Elixir deliberately using erlang macro-based logger interface without passing in call location", data: warning)

  expected_counts = Map.put(expected_counts, 1, 1)

  defp filter_warning(warning = {:warn_opaque, {'lib/mix/tasks/test.ex', _}, {:opaque_match, ['pattern \#{\'__struct__\':=\'Elixir.MapSet\'}', '\'Elixir.MapSet\':t(binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | char(),binary() | []))', '\'Elixir.MapSet\':t(binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | char(),binary() | []))']}}),
    do: filtered(id: 2, comment: "Elixir folks want to be able to pattern match on a struct name while keeping the struct type opaque", data: warning)

  expected_counts = Map.put(expected_counts, 2, 1)

  defp filter_warning(warning = {:warn_matching, {'lib/kernel.ex', _}, {:pattern_match, ['pattern \'false\'', '\'true\'']}}),
    do: filtered(id: 3, comment: "inlined bootstrap check stuff", data: warning)

  expected_counts = Map.put(expected_counts, 3, 8)

  defp filter_warning(warning = {:warn_matching, {'src/elixir_erl_compiler.erl', 59}, {:pattern_match, _lots_of_details}}),
    do: filtered(id: 4, comment: "return type not documented in erlang", data: warning)

  expected_counts = Map.put(expected_counts, 4, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/mix/tasks/test.ex', _}, {:no_return, [:only_normal, :raise_with_shell, 2]}}),
    do: filtered(id: 5, comment: "not annotated exception", data: warning)

  expected_counts = Map.put(expected_counts, 5, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/mix/release.ex', _}, {:no_return, [:only_normal, :bad_umbrella!, 0]}}),
    do: filtered(id: 6, comment: "not annotated exception", data: warning)

  expected_counts = Map.put(expected_counts, 6, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/mix/dep/loader.ex', _}, {:no_return, [:only_normal, :invalid_dep_format, 1]}}),
    do: filtered(id: 7, comment: "not annotated exception", data: warning)

  expected_counts = Map.put(expected_counts, 7, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/iex.ex', _}, {:no_return, [:only_normal, :__break__!, 2]}}),
    do: filtered(id: 8, comment: "not annotated exception", data: warning)

  expected_counts = Map.put(expected_counts, 8, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'src/elixir_quote.erl', _}, {:no_return, [:only_normal, :bad_escape, 1]}}),
    do: filtered(id: 9, comment: "not annotated exception", data: warning)

  expected_counts = Map.put(expected_counts, 9, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/mix/tasks/iex.ex', _}, {:no_return, [:only_normal, :run, 1]}}),
    do: filtered(id: 10, comment: "not annotated exception", data: warning)

  expected_counts = Map.put(expected_counts, 10, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/iex/cli.ex', _}, {:no_return, [:only_normal]}}),
    do: filtered(id: 11, comment: "not annotated exit", data: warning)

  expected_counts = Map.put(expected_counts, 11, 1)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/gen_event.ex', _}, {:no_return, [:only_normal, :system_terminate, 4]}}),
    do: filtered(id: 12, comment: "not annotated exit", data: warning)

  expected_counts = Map.put(expected_counts, 12, 1)

  @yecc_yrl_functions [:error_bad_keyword_call_follow_up, :error_bad_keyword_data_follow_up, :return_error, :error_invalid_stab, :error_bad_atom, :error_no_parens_strict, :error_no_parens_many_strict, :error_no_parens_container_strict, :error_invalid_kw_identifier]
  @yecc_erl_clauses [:yeccpars2_87, :yeccpars2_89, :yeccpars2_108, :yeccpars2_199, :yeccpars2_218, :yeccpars2_230, :yeccpars2_310, :yeccpars2_367, :yeccpars2_402, :yeccpars2_404]
  yecc_yrl_clauses = Enum.map(@yecc_erl_clauses, fn atom -> :"#{atom}_" end)
  @yecc_yrl_clauses yecc_yrl_clauses

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/elixir/src/elixir_parser.yrl', _}, {:no_return, [:only_normal, function, _arity]}}) when function in @yecc_yrl_functions or function in @yecc_yrl_clauses,
    do: filtered(id: 13, comment: "parser not annotated exception", data: warning)

  expected_counts = Map.put(expected_counts, 13, 19)

  defp filter_warning(warning = {:warn_return_no_exit, {'lib/elixir/src/elixir_parser.erl', _}, {:no_return, [:only_normal, function, _arity]}}) when function in @yecc_erl_clauses,
    do: filtered(id: 14, comment: "parser not annotated exception", data: warning)

  expected_counts = Map.put(expected_counts, 14, 6)

  defp filter_warning(warning = {:warn_unknown, {[], 0}, {:unknown_function, {module, :__impl__, 1}}}) when module in [Collectable.Atom, Collectable.Float, Collectable.Function, Collectable.Integer, Collectable.PID, Collectable.Port, Collectable.Reference, Collectable.Tuple, Enumerable.Atom, Enumerable.BitString, Enumerable.Float, Enumerable.Integer, Enumerable.PID, Enumerable.Port, Enumerable.Reference, Enumerable.Tuple, List.Chars.Function, List.Chars.Map, List.Chars.PID, List.Chars.Port, List.Chars.Reference, List.Chars.Tuple, String.Chars.Function, String.Chars.Map, String.Chars.PID, String.Chars.Port, String.Chars.Reference, String.Chars.Tuple],
    do: filtered(id: 15, comment: "some protocol consolidation stuff", data: warning)

  expected_counts = Map.put(expected_counts, 15, 28)

  defp filter_warning(warning = {:warn_unknown, {[], 0}, {:unknown_function, {Hex, function, 0}}}) when function in [:start, :version],
    do: filtered(id: 16, comment: "Hex package loading gets handled by the Mix task", data: warning)

  expected_counts = Map.put(expected_counts, 16, 2)

  defp filter_warning(warning = {:warn_not_called, {'lib/base.ex', _}, {:unused_fun, [function, _]}}) when function in [:encode_pair_clauses, :shift, :encode_clauses, :decode_char_clauses, :decode_mixed_clauses, :decode_clauses, :bad_digit_clause],
    do: filtered(id: 17, comment: "functions inlined or only used to generate other functions at compile time", data: warning)

  expected_counts = Map.put(expected_counts, 17, 7)

  defp filter_warning(warning = {:warn_not_called, {'lib/system.ex', _}, {:unused_fun, [function, 1]}}) when function in [:read_stripped, :strip],
    do: filtered(id: 18, comment: "functions called only during elixir compilation time", data: warning)

  expected_counts = Map.put(expected_counts, 18, 2)

  defp filter_warning(warning = {:warn_matching, {'lib/dynamic_supervisor.ex', 350}, {:pattern_match_cov, ['variable _other@1', '{_,_,_,_,_,_} | \#{\'id\':=_, \'start\':={atom(),atom(),[any()]}, \'modules\'=>\'dynamic\' | [atom()], \'restart\'=>\'permanent\' | \'temporary\' | \'transient\', \'shutdown\'=>\'brutal_kill\' | \'infinity\' | non_neg_integer(), \'type\'=>\'supervisor\' | \'worker\'}']}}),
    do: filtered(id: 19, comment: "overly defensive code", data: warning)

  expected_counts = Map.put(expected_counts, 19, 1)

  defp filter_warning(warning = {:warn_matching, {'lib/string_io.ex', 410}, {:guard_fail, [:is_list, '(_data@1::binary())']}}),
    do: filtered(id: 20, comment: "overly defensive code", data: warning)

  expected_counts = Map.put(expected_counts, 20, 1)

  defp filter_warning(warning = {:warn_matching, {'lib/mix/utils.ex', 741}, {:pattern_match, ['pattern \'nil\'', '\#{\'__struct__\':=\'Elixir.URI\', \'authority\':=\'nil\' | binary(), \'fragment\':=\'nil\' | binary(), \'host\':=\'nil\' | binary(), \'path\':=\'nil\' | binary(), \'port\':=\'nil\' | char(), \'query\':=\'nil\' | binary(), \'scheme\':=\'nil\' | binary(), \'userinfo\':=\'nil\' | binary()}']}}),
    do: filtered(id: 21, comment: "overly defensive code", data: warning)

  expected_counts = Map.put(expected_counts, 21, 1)

  defp filter_warning(warning = {:warn_matching, {'lib/iex/helpers.ex', 605}, {:pattern_match, ['pattern <__key@1, \'nil\'>', '<<<_:64,_:_*8>>,<<_:80>> | string() | non_neg_integer()>']}}),
    do: filtered(id: 22, comment: "overly_defensive code", data: warning)

  expected_counts = Map.put(expected_counts, 22, 1)

  defp filter_warning(warning = {:warn_matching, {'lib/mix/tasks/deps.compile.ex', 269}, {:guard_fail, ['_@6::\'true\'', '=:=', '\'nil\'']}}),
    do: filtered(id: 23, comment: "slightly dead code", data: warning)

  expected_counts = Map.put(expected_counts, 23, 1)

  defp filter_warning(warning = {:warn_matching, {'lib/calendar/time.ex', 636}, {:pattern_match, ['pattern {\'error\', _reason@1}', '{\'ok\',\#{\'__struct__\':=\'Elixir.Time\', \'calendar\':=atom(), \'hour\':=non_neg_integer(), \'microsecond\':={non_neg_integer(),non_neg_integer()}, \'minute\':=non_neg_integer(), \'second\':=non_neg_integer()}}']}}),
    do: filtered(id: 24, comment: "slightly dead code", data: warning)

  expected_counts = Map.put(expected_counts, 24, 1)

  defp filter_warning(warning = {:warn_callgraph, {'lib/system.ex', _}, {:call_to_missing, [:os, :env, 0]}}),
    do: filtered(id: 25, comment: "OTP 24 function", data: warning)

  expected_counts = Map.put(expected_counts, 25, 1)

  defp filter_warning(warning), do: unfiltered(data: warning)

  @expected_counts expected_counts
  defp expected_counts(), do: @expected_counts

  def run() do
    results =
      :dialyzer.run(
        init_plt: 'plt-dir/otp.plt',
        warnings: [
          :unknown,
          :no_improper_lists
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
      |> filter()

    actual_counts =
      results
      |> Enum.reduce(%{}, fn
        filtered(id: id), actual_counts ->
          Map.update(actual_counts, id, 1, &(&1 + 1))

        _, actual_counts ->
          actual_counts
      end)

    unexpected_counts_warnings =
      expected_counts()
      |> Enum.filter(fn
        {id, expected_count} ->
          actual_counts[id] != expected_count
      end)
      |> Enum.map(fn
        {id, expected_count} ->
          unexpected_count(id: id, actual: actual_counts[id], expected: expected_count)
      end)

    results =
      (unexpected_counts_warnings ++ results)
      |> Enum.sort_by(fn
        unfiltered() -> 1
        unexpected_count() -> 2
        filtered() -> 3
      end)

    has_potential_issues? =
      case results do
        [unfiltered() | _] -> true
        [unexpected_count() | _] -> true
        _ -> false
      end

    results
    |> Stream.map(fn
      unfiltered(data: warning) ->
        formatted_warning =
          warning
          |> :dialyzer.format_warning(indent_opt: true, filename_opt: :fullpath)
          |> to_string()

        "############################################################\n" <>
          "Potential issue: \n#{formatted_warning}\n\n" <>
          "data: #{inspect(warning)}\n\n"

      unexpected_count(id: id, actual: actual_count, expected: expected_count) ->
        "############################################################\n" <>
          "Expected count of filtered non-issues ##{id} is off! Expected #{expected_count}, found #{actual_count || 0} issues\n\n"

      filtered(id: id, comment: comment, data: warning) ->
        formatted_warning =
          warning
          |> :dialyzer.format_warning(indent_opt: true, filename_opt: :fullpath)
          |> to_string()

        "############################################################\n" <>
          "Filtered non-issue ##{id} (#{comment}):\n#{formatted_warning}\n\n" <>
          "data: #{inspect(warning)}\n\n"
    end)
    |> Stream.into(File.stream!("report", [:write, :utf8]))
    |> Stream.into(IO.stream(:stdio, :line))
    |> Stream.run()

    if(has_potential_issues?) do
      System.halt(1)
    end
  end
end

Dialyze.run()

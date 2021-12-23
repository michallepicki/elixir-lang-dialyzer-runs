defmodule Dialyzer do
  require Record

  Record.defrecord(:filtered, id: nil, comment: nil, data: nil)
  Record.defrecord(:unfiltered, data: nil)
  Record.defrecord(:unexpected_count, id: nil, actual: nil, expected: nil)

  expected_counts = %{}

  defp filter([], acc), do: Enum.reverse(acc)

  defp filter([warning | rest], acc), do: filter(rest, [filter(warning) | acc])

  @id 10
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)
  # discussed in https://github.com/elixir-lang/elixir/issues/10279
  # and https://github.com/elixir-lang/elixir/pull/10280
  # mostly fixed in https://github.com/elixir-lang/elixir/pull/10287
  # may be resolved in https://github.com/elixir-lang/elixir/issues/9465
  defp filter(expected = {:warn_failing_call, {'lib/logger.ex', _}, {:call, [Logger, :__do_log__, _, [3], :only_sig, _, _, {false, :none}]}}),
    do: filtered(comment: "Elixir deliberately using erlang macro-based logger interface without passing in call location", id: @id, data: expected)

  @id 20
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)
  # discussed in https://github.com/elixir-lang/elixir/pull/9979#discussion_r415730426
  # and https://github.com/elixir-lang/elixir/pull/9993
  # and https://github.com/elixir-lang/elixir/pull/9995
  defp filter(expected = {:warn_opaque, {'lib/mix/tasks/test.ex', _}, {:opaque_match, ['pattern \#{\'__struct__\':=\'Elixir.MapSet\'}', '\'Elixir.MapSet\':t(binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | char(),binary() | []))', '\'Elixir.MapSet\':t(binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | char(),binary() | []))']}}),
    do: filtered(comment: "Elixir folks want to be able to pattern match on a struct name while keeping the struct type opaque", id: @id, data: expected)

  @id 30
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  @line_file_match (case System.otp_release() >= "24" do
                      true ->
                        quote do
                          {59, _}
                        end

                      false ->
                        quote do
                          59
                        end
                    end)
  # discussed in https://github.com/elixir-lang/elixir/pull/9979#discussion_r416206411
  defp filter(expected = {:warn_matching, {'src/elixir_erl_compiler.erl', unquote(@line_file_match)}, {:pattern_match, _lots_of_details}}),
    do: filtered(comment: "return type not documented in erlang", id: @id, data: expected)

  @id 40
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)
  # discussed in https://github.com/elixir-lang/elixir/issues/11092
  defp filter(expected = {:warn_matching, {'lib/calendar/time.ex', 636}, {:pattern_match, ['pattern {\'error\', _reason@1}', '{\'ok\',\#{\'__struct__\':=\'Elixir.Time\', \'calendar\':=atom(), \'hour\':=non_neg_integer(), \'microsecond\':={non_neg_integer(),non_neg_integer()}, \'minute\':=non_neg_integer(), \'second\':=non_neg_integer()}}']}}),
    do: filtered(comment: "slightly dead code", id: @id, data: expected)

  @id 50
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  @pattern_match_cov_value (case System.otp_release() >= "23" do
    true ->
      ['pattern _date_range@1 = \#{\'__struct__\':=\'Elixir.Date.Range\', \'first_in_iso_days\':=_first_days@1, \'last_in_iso_days\':=_last_days@1}', '\#{\'__struct__\':=\'Elixir.Date.Range\', \'first\':=\#{\'calendar\':=_, _=>_}, \'first_in_iso_days\':=_, \'last_in_iso_days\':=_, \'step\':=_, _=>_}']
    false ->
      ['pattern _date_range@1 = \#{\'last_in_iso_days\':=_last_days@1, \'first_in_iso_days\':=_first_days@1, \'__struct__\':=\'Elixir.Date.Range\'}', '\#{\'__struct__\':=\'Elixir.Date.Range\', \'first\':=\#{\'calendar\':=_, _=>_}, \'first_in_iso_days\':=_, \'last_in_iso_days\':=_, \'step\':=_, _=>_}']
  end)

  defp filter(expected = {:warn_matching, {'lib/calendar/date_range.ex', 201}, {:pattern_match_cov, @pattern_match_cov_value}}),
    do: filtered(comment: "code added for backwards compatibility with old date ranges without step field", id: @id, data: expected)

  @id 60
  @counts 2
  expected_counts = Map.put(expected_counts, @id, @counts)

  @file_match (case System.otp_release() >= "24" do
                 true -> 'lib/mix/hex.ex'
                 false -> ''
               end)
  @line_match (case System.otp_release() >= "24" do
                 true -> [40, 59]
                 false -> [0]
               end)

  defp filter(expected = {:warn_unknown, {@file_match, lines}, {:unknown_function, {Hex, function, 0}}}) when function in [:start, :version] and lines in @line_match,
    do: filtered(comment: "Hex package loading gets handled by the Mix task", id: @id, data: expected)

  @id 70
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_matching, {'lib/dynamic_supervisor.ex', 425}, {:pattern_match_cov, ['variable _other@1', '{_,_,_,_,_,_} | \#{\'id\':=_, \'start\':={atom(),atom(),[any()]}, \'modules\'=>\'dynamic\' | [atom()], \'restart\'=>\'permanent\' | \'temporary\' | \'transient\', \'shutdown\'=>\'brutal_kill\' | \'infinity\' | non_neg_integer(), \'type\'=>\'supervisor\' | \'worker\'}']}}),
    do: filtered(comment: "overly defensive code", id: @id, data: expected)

  @id 80
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_matching, {'lib/string_io.ex', 410}, {:guard_fail, [:is_list, '(_data@1::binary())']}}),
    do: filtered(comment: "overly defensive code", id: @id, data: expected)

  @id 90
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_matching, {'lib/mix/utils.ex', 755}, {:pattern_match, ['pattern \'nil\'', '\#{\'__struct__\':=\'Elixir.URI\', \'authority\':=\'Elixir.URI\':authority(), \'fragment\':=\'nil\' | binary(), \'host\':=\'nil\' | binary(), \'path\':=\'nil\' | binary(), \'port\':=\'nil\' | char(), \'query\':=\'nil\' | binary(), \'scheme\':=\'nil\' | binary(), \'userinfo\':=\'nil\' | binary()}']}}),
    do: filtered(comment: "overly defensive code", id: @id, data: expected)

  @id 100
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_matching, {'lib/iex/helpers.ex', 624}, {:pattern_match, ['pattern <__key@1, \'nil\'>', '<<<_:64,_:_*8>>,<<_:80>> | string() | non_neg_integer()>']}}),
    do: filtered(comment: "overly_defensive code", id: @id, data: expected)

  @id 110
  @counts 22
  expected_counts = Map.put(expected_counts, @id, @counts)

  @file_match (case System.otp_release() >= "24" do
                 true -> ['lib/collectable.ex', 'lib/enum.ex', 'lib/list/chars.ex']
                 false -> ['']
               end)
  @line_match (case System.otp_release() >= "24" do
                 true -> 1
                 false -> 0
               end)

  defp filter(expected = {:warn_unknown, {file, line}, {:unknown_function, {module, :__impl__, 1}}}) when module in [Collectable.Atom, Collectable.Float, Collectable.Function, Collectable.Integer, Collectable.PID, Collectable.Port, Collectable.Reference, Collectable.Tuple, Enumerable.Atom, Enumerable.BitString, Enumerable.Float, Enumerable.Integer, Enumerable.PID, Enumerable.Port, Enumerable.Reference, Enumerable.Tuple, List.Chars.Function, List.Chars.Map, List.Chars.PID, List.Chars.Port, List.Chars.Reference, List.Chars.Tuple] and file in @file_match and line == @line_match,
    do: filtered(comment: "some protocol consolidation stuff", id: @id, data: expected)

  @id 120
  @counts 6
  expected_counts = Map.put(expected_counts, @id, @counts)

  @file_match (case System.otp_release() >= "24" do
                 true -> 'lib/string/chars.ex'
                 false -> ''
               end)
  @line_match (case System.otp_release() >= "24" do
                 true -> 3
                 false -> 0
               end)

  defp filter(expected = {:warn_unknown, {@file_match, @line_match}, {:unknown_function, {module, :__impl__, 1}}}) when module in [String.Chars.Function, String.Chars.Map, String.Chars.PID, String.Chars.Port, String.Chars.Reference, String.Chars.Tuple],
    do: filtered(comment: "some protocol consolidation stuff", id: @id, data: expected)

  @id 130
  @counts 6
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_matching, {'lib/kernel.ex', _}, {:pattern_match, ['pattern \'false\'', '\'true\'']}}),
    do: filtered(comment: "inlined bootstrap check stuff", id: @id, data: expected)

  @id 140
  @counts 7
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_not_called, {'lib/base.ex', _}, {:unused_fun, [function, _]}}) when function in [:encode_pair_clauses, :shift, :encode_clauses, :decode_char_clauses, :decode_mixed_clauses, :decode_clauses, :bad_digit_clause],
    do: filtered(comment: "functions inlined or only used to generate other functions at compile time", id: @id, data: expected)

  @id 150
  @counts 2
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_not_called, {'lib/system.ex', _}, {:unused_fun, [function, 1]}}) when function in [:read_stripped, :strip],
    do: filtered(comment: "functions called only during elixir compilation time", id: @id, data: expected)

  @id 160
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_return_no_exit, {'lib/mix/tasks/test.ex', _}, {:no_return, [:only_normal, :raise_with_shell, 2]}}),
    do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 170
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_return_no_exit, {'lib/mix/release.ex', _}, {:no_return, [:only_normal, :bad_umbrella!, 0]}}),
    do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 180
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_return_no_exit, {'lib/mix/dep/loader.ex', _}, {:no_return, [:only_normal, :invalid_dep_format, 1]}}),
    do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 190
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_return_no_exit, {'lib/mix/scm/path.ex', 61}, {:no_return, [:only_normal, :checkout, 1]}}),
    do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 200
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_return_no_exit, {'lib/iex.ex', _}, {:no_return, [:only_normal, :__break__!, 2]}}),
    do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 210
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_return_no_exit, {'src/elixir_quote.erl', _}, {:no_return, [:only_normal, :bad_escape, 1]}}),
    do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 220
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_return_no_exit, {'lib/mix/tasks/iex.ex', _}, {:no_return, [:only_normal, :run, 1]}}),
    do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 230
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_return_no_exit, {'lib/iex/cli.ex', _}, {:no_return, [:only_normal]}}),
    do: filtered(comment: "not annotated exit", id: @id, data: expected)

  @id 240
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_return_no_exit, {'lib/kernel/cli.ex', 221}, {:no_return, [:only_normal, :halt_standalone, 1]}}),
    do: filtered(comment: "not annotated exit", id: @id, data: expected)

  @id 250
  @counts 1
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_return_no_exit, {'lib/gen_event.ex', _}, {:no_return, [:only_normal, :system_terminate, 4]}}),
    do: filtered(comment: "not annotated exit", id: @id, data: expected)

  @yecc_yrl_functions [:return_error, :error_invalid_stab, :error_bad_atom, :error_no_parens_strict, :error_no_parens_many_strict, :error_no_parens_container_strict, :error_invalid_kw_identifier, :return_error_with_meta]

  @id 260
  @counts 8
  expected_counts = Map.put(expected_counts, @id, @counts)

  defp filter(expected = {:warn_return_no_exit, {'lib/elixir/src/elixir_parser.yrl', _}, {:no_return, [:only_normal, function, _arity]}}) when function in @yecc_yrl_functions,
    do: filtered(comment: "parser not annotated exception", id: @id, data: expected)

  @id 270
  @counts 1
  expected_counts =
    if System.otp_release() >= "24" do
      expected_counts
    else
      Map.put(expected_counts, @id, @counts)
    end
  if System.otp_release() >= "24" do
    :ok
  else
    defp filter(expected = {:warn_callgraph, {'lib/task.ex', _}, {:call_to_missing, [:erlang, :monitor, 3]}}),
      do: filtered(comment: "run-time function_exported? check", id: @id, data: expected)
  end

  @id 280
  @counts 1
  expected_counts =
    if System.otp_release() >= "24" do
      expected_counts
    else
      Map.put(expected_counts, @id, @counts)
    end
  if System.otp_release() >= "24" do
    :ok
  else
    defp filter(expected = {:warn_callgraph, {'lib/task/supervisor.ex', _}, {:call_to_missing, [:erlang, :monitor, 3]}}),
      do: filtered(comment: "run-time function_exported? check", id: @id, data: expected)
  end

  @id 290
  @counts 1
  expected_counts =
    if System.otp_release() >= "24" do
      expected_counts
    else
      Map.put(expected_counts, @id, @counts)
    end
  if System.otp_release() >= "24" do
    :ok
  else
    defp filter(expected = {:warn_callgraph, {'lib/system.ex', _}, {:call_to_missing, [:os, :env, 0]}}),
      do: filtered(comment: "run-time function_exported? check", id: @id, data: expected)
  end

  @id 300
  @counts 1
  expected_counts =
    if System.otp_release() >= "24" do
      expected_counts
    else
      Map.put(expected_counts, @id, @counts)
    end
  if System.otp_release() >= "24" do
    :ok
  else
    defp filter(expected = {:warn_callgraph, {'lib/map.ex', _}, {:call_to_missing, [:maps, :from_keys, 2]}}),
      do: filtered(comment: "run-time function_exported? check", id: @id, data: expected)
  end

  @id 310
  @counts 1
  expected_counts =
    if System.otp_release() >= "23" do
      expected_counts
    else
      Map.put(expected_counts, @id, @counts)
    end
  if System.otp_release() >= "23" do
    :ok
  else
    defp filter(expected = {:warn_callgraph, {'lib/config/provider.ex', _}, {:call_to_missing, [:init, :restart, 1]}}),
      do: filtered(comment: "run-time otp version check", id: @id, data: expected)
  end

  @id 320
  @counts 1
  expected_counts =
    if System.otp_release() >= "23" do
      expected_counts
    else
      Map.put(expected_counts, @id, @counts)
    end
  if System.otp_release() >= "23" do
    :ok
  else
    defp filter(expected = {:warn_matching, {'lib/module/types/of.ex', 290}, {:guard_fail_pat, ['pattern {\'deprecated\', _string@2, _removal@1}', '\'no\' | {\'removed\',[1..255,...]} | {\'deprecated\',{\'calendar\' | \'erlang\' | \'gen_statem\' | \'net_adm\' | \'queue\' | \'rand\' | \'rpc\' | \'slave\',atom(),0 | 1 | 2 | 3 | 4 | 5 | 6},[32 | 97 | 101 | 102 | 108 | 114 | 115 | 116 | 117,...]} | {\'removed\',{\'cerl\' | \'core_lib\' | \'crypto\' | \'erl_anno\' | \'erl_parse\' | \'erlang\' | \'rpc\' | \'unicode\',atom(),0 | 1 | 2 | 3 | 4 | 5},[32 | 46 | 48 | 49 | 50 | 57 | 79 | 80 | 84,...]}']}}),
      do: filtered(comment: "elixir doesn't print deprecated functions info for otp 22", id: @id, data: expected)
  end
 
  defp filter(warning),
    do: unfiltered(data: warning)

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
      |> filter([])

    counts =
      results
      |> Enum.reduce(%{}, fn
        filtered(id: id), counts ->
          Map.update(counts, id, 1, &(&1 + 1))

        _, counts ->
          counts
      end)

    unexpected_counts_warnings =
      expected_counts()
      |> Enum.filter(fn
        {id, expected_count} ->
          counts[id] != expected_count
      end)
      |> Enum.map(fn
        {id, expected_count} ->
          unexpected_count(id: id, actual: counts[id], expected: expected_count)
      end)

    results =
      (unexpected_counts_warnings ++ results)
      |> Enum.sort_by(fn
        filtered() -> 1
        unexpected_count() -> 2
        unfiltered(data: {:warn_unknown, _, _}) -> 3
        unfiltered(data: {:warn_return_no_exit, _, _}) -> 4
        unfiltered() -> 5
      end)

    has_potential_issues? =
      case Enum.reverse(results) do
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
          "data: #{inspect(warning, limit: :infinity, printable_limit: :infinity, width: :infinity)}\n\n"

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
          "data: #{inspect(warning, limit: :infinity, printable_limit: :infinity, width: :infinity)}\n\n"
    end)
    |> Stream.into(File.stream!("report", [:write, :utf8]))
    |> Stream.into(IO.stream(:stdio, :line))
    |> Stream.run()

    if(has_potential_issues?) do
      System.halt(1)
    end
  end
end

Dialyzer.run()

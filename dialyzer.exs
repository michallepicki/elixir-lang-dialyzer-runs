defmodule Dialyzer do
  require Record

  Record.defrecord(:filtered, id: nil, comment: nil, data: nil)
  Record.defrecord(:unfiltered, data: nil)
  Record.defrecord(:unexpected_count, id: nil, actual: nil, expected: nil)

  def run() do
    otp_version =
      File.read!(
        :filename.join([
          :code.root_dir(),
          'releases',
          :erlang.system_info(:otp_release),
          'OTP_VERSION'
        ])
      )
      |> String.trim()
      |> String.to_charlist()

    plt_filename = 'plt-dir/' ++ otp_version ++ '.plt'

    dirs =
      :dialyzer_cl_parse.get_lib_dir([
        'erts',
        'kernel',
        'stdlib',
        'compiler',
        'syntax_tools',
        'parsetools',
        'tools',
        'ssl',
        'inets',
        'crypto',
        'runtime_tools',
        'ftp',
        'tftp',
        'mnesia',
        'public_key',
        'asn1',
        'sasl'
      ])

    if !File.exists?(plt_filename) do
      :dialyzer.run(
        analysis_type: :plt_build,
        output_plt: plt_filename,
        files_rec: dirs
      )
    end

    results =
      :dialyzer.run(
        init_plt: plt_filename,
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
      |> filter_results([])

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
          (counts[id] || 0) != expected_count
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
      case List.last(results) do
        unfiltered() -> true
        unexpected_count() -> true
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

    if has_potential_issues? do
      System.halt(1)
    end
  end

  expected_counts = %{}

  defp filter_results([], acc), do: acc

  defp filter_results([warning | rest], acc), do: filter_results(rest, [filter(warning) | acc])

  @id 10
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)
  # discussed in https://github.com/elixir-lang/elixir/issues/10279
  # and https://github.com/elixir-lang/elixir/pull/10280
  # mostly fixed in https://github.com/elixir-lang/elixir/pull/10287
  # may be resolved in https://github.com/elixir-lang/elixir/issues/9465
  defp filter(
         expected =
           {:warn_failing_call, {'lib/logger.ex', 866},
            {:call, [Logger, :__do_log__, _, [3], :only_sig, _, _, {false, :none}]}}
       ),
       do:
         filtered(
           comment:
             "Elixir deliberately using erlang macro-based logger interface without passing in call location",
           id: @id,
           data: expected
         )

  @id 30
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  @line_file_match if System.otp_release() >= "24", do: quote(do: {59, _}), else: quote(do: 59)
  # discussed in https://github.com/elixir-lang/elixir/pull/9979#discussion_r416206411
  defp filter(
         expected =
           {:warn_matching, {'src/elixir_erl_compiler.erl', unquote(@line_file_match)},
            {:pattern_match, _lots_of_details}}
       ),
       do: filtered(comment: "return type not documented in erlang", id: @id, data: expected)

  @id 40
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)
  # discussed in https://github.com/elixir-lang/elixir/issues/11092
  defp filter(
         expected =
           {:warn_matching, {'lib/calendar/time.ex', 672},
            {:pattern_match,
             [
               'pattern {\'error\', _reason@1}',
               '{\'ok\',\#{\'__struct__\':=\'Elixir.Time\', \'calendar\':=atom(), \'hour\':=non_neg_integer(), \'microsecond\':={non_neg_integer(),non_neg_integer()}, \'minute\':=non_neg_integer(), \'second\':=non_neg_integer()}}'
             ]}}
       ),
       do: filtered(comment: "slightly dead code", id: @id, data: expected)

  @id 50
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_matching, {'lib/calendar/date_range.ex', 201},
            {:pattern_match_cov,
             [
               'pattern _date_range@1 = \#{\'__struct__\':=\'Elixir.Date.Range\', \'first_in_iso_days\':=_first_days@1, \'last_in_iso_days\':=_last_days@1}',
               '\#{\'__struct__\':=\'Elixir.Date.Range\', \'first\':=\#{\'calendar\':=_, _=>_}, \'first_in_iso_days\':=_, \'last_in_iso_days\':=_, \'step\':=_, _=>_}'
             ]}}
       ),
       do:
         filtered(
           comment:
             "code added for backwards compatibility with old date ranges without step field",
           id: @id,
           data: expected
         )

  @id 60
  @count 2
  expected_counts = Map.put(expected_counts, @id, @count)

  @file_match if System.otp_release() >= "24", do: 'lib/mix/hex.ex', else: ''
  @line_match if System.otp_release() >= "24", do: [40, 59], else: [0]

  defp filter(
         expected = {:warn_unknown, {@file_match, lines}, {:unknown_function, {Hex, function, 0}}}
       )
       when function in [:start, :version] and lines in @line_match,
       do:
         filtered(
           comment: "Hex package loading gets handled by the Mix task",
           id: @id,
           data: expected
         )

  @id 70
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_matching, {'lib/dynamic_supervisor.ex', 428},
            {:pattern_match_cov,
             [
               'variable _other@1',
               '{_,_,_,_,_,_} | \#{\'id\':=_, \'start\':={atom(),atom(),[any()]}, \'modules\'=>\'dynamic\' | [atom()], \'restart\'=>\'permanent\' | \'temporary\' | \'transient\', \'shutdown\'=>\'brutal_kill\' | \'infinity\' | pos_integer(), \'type\'=>\'supervisor\' | \'worker\'}'
             ]}}
       ),
       do: filtered(comment: "overly defensive code", id: @id, data: expected)

  @id 90
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_matching, {'lib/mix/utils.ex', 756},
            {:pattern_match,
             [
               'pattern \'nil\'',
               '\#{\'__struct__\':=\'Elixir.URI\', \'authority\':=\'Elixir.URI\':authority(), \'fragment\':=\'nil\' | binary(), \'host\':=\'nil\' | binary(), \'path\':=\'nil\' | binary(), \'port\':=\'nil\' | char(), \'query\':=\'nil\' | binary(), \'scheme\':=\'nil\' | binary(), \'userinfo\':=\'nil\' | binary()}'
             ]}}
       ),
       do: filtered(comment: "overly defensive code", id: @id, data: expected)

  @id 100
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_matching, {'lib/iex/helpers.ex', 633},
            {:pattern_match,
             [
               'pattern <__key@1, \'nil\'>',
               '<<<_:64,_:_*8>>,binary() | string() | non_neg_integer()>'
             ]}}
       ),
       do: filtered(comment: "overly_defensive code", id: @id, data: expected)

  @id 110
  @count 22
  expected_counts = Map.put(expected_counts, @id, @count)

  @file_match if System.otp_release() >= "24",
                do: ['lib/collectable.ex', 'lib/enum.ex', 'lib/list/chars.ex'],
                else: ['']
  @line_match if System.otp_release() >= "24", do: 1, else: 0

  defp filter(
         expected = {:warn_unknown, {file, line}, {:unknown_function, {module, :__impl__, 1}}}
       )
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
              List.Chars.Tuple
            ] and file in @file_match and line == @line_match,
       do: filtered(comment: "some protocol consolidation stuff", id: @id, data: expected)

  @id 120
  @count 6
  expected_counts = Map.put(expected_counts, @id, @count)

  @file_match if System.otp_release() >= "24", do: 'lib/string/chars.ex', else: ''
  @line_match if System.otp_release() >= "24", do: 3, else: 0

  defp filter(
         expected =
           {:warn_unknown, {@file_match, @line_match},
            {:unknown_function, {module, :__impl__, 1}}}
       )
       when module in [
              String.Chars.Function,
              String.Chars.Map,
              String.Chars.PID,
              String.Chars.Port,
              String.Chars.Reference,
              String.Chars.Tuple
            ],
       do: filtered(comment: "some protocol consolidation stuff", id: @id, data: expected)

  @id 130
  @count 5
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_matching, {'lib/kernel.ex', line},
            {:pattern_match, ['pattern \'false\'', '\'true\'']}}
       )
       when line in [1950, 3455, 3818, 3878, 4258],
       do: filtered(comment: "inlined bootstrap check stuff", id: @id, data: expected)

  @id 150
  @count 2
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(expected = {:warn_not_called, {'lib/system.ex', _}, {:unused_fun, [function, 1]}})
       when function in [:read_stripped, :strip],
       do:
         filtered(
           comment: "functions called only during elixir compilation time",
           id: @id,
           data: expected
         )

  @id 160
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_return_no_exit, {'lib/mix/tasks/test.ex', 582},
            {:no_return, [:only_normal, :raise_with_shell, 2]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 170
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_return_no_exit, {'lib/mix/release.ex', _},
            {:no_return, [:only_normal, :bad_umbrella!, 0]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 180
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_return_no_exit, {'lib/mix/dep/loader.ex', 258},
            {:no_return, [:only_normal, :invalid_dep_format, 1]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 190
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_return_no_exit, {'lib/mix/scm/path.ex', 61},
            {:no_return, [:only_normal, :checkout, 1]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 200
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_return_no_exit, {'lib/iex.ex', _}, {:no_return, [:only_normal, :__break__!, 2]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 210
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_return_no_exit, {'src/elixir_quote.erl', _},
            {:no_return, [:only_normal, :bad_escape, 1]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 211
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_return_no_exit, {'lib/mix/dep/converger.ex', 55},
            {:no_return, [:only_normal, :cycle_found, 1]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 220
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_return_no_exit, {'lib/mix/tasks/iex.ex', 9},
            {:no_return, [:only_normal, :run, 1]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 221
  @count 1
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, @count),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         expected =
           {:warn_return_no_exit, {'lib/config/provider.ex', 403},
            {:no_return, [:only_normal, :bad_path_abort, 2]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 222
  @count 4
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, @count),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         expected =
           {:warn_return_no_exit, {'src/elixir_clauses.erl', location},
            {:no_return, [:only_normal]}}
       )
       when location in [{95, 43}, {114, 43}, {133, 16}, {227, 16}],
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 223
  @count 1
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, @count),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         expected =
           {:warn_return_no_exit, {'src/elixir_erl.erl', {583, 1}},
            {:no_return, [:only_normal, :form_error, 2]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 224
  @count 2
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, @count),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         expected =
           {:warn_return_no_exit, {'src/elixir_erl_compiler.erl', location},
            {:no_return, [:only_normal]}}
       )
       when location in [{78, 21}, {80, 21}],
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 225
  @count 1
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, @count),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         expected =
           {:warn_return_no_exit, {'src/elixir_erl_compiler.erl', {113, 1}},
            {:no_return, [:only_normal, :handle_file_error, 2]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 226
  @count 1
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, @count),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         expected =
           {:warn_return_no_exit, {'src/elixir_errors.erl', {182, 1}},
            {:no_return, [:only_normal, :raise_reserved, 3]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 227
  @count 2
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, @count),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         expected =
           {:warn_return_no_exit, {'src/elixir_errors.erl', location},
            {:no_return, [:only_normal, :raise, arity]}}
       )
       when location in [{216, 1}, {219, 1}] and arity in [5, 4],
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 228
  @count 1
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, @count),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         expected =
           {:warn_return_no_exit, {'src/elixir_fn.erl', {121, 1}},
            {:no_return, [:only_normal, :invalid_capture, 3]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: expected)

  @id 230
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected = {:warn_return_no_exit, {'lib/iex/cli.ex', 151}, {:no_return, [:only_normal]}}
       ),
       do: filtered(comment: "not annotated exit", id: @id, data: expected)

  @id 240
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_return_no_exit, {'lib/kernel/cli.ex', 221},
            {:no_return, [:only_normal, :halt_standalone, 1]}}
       ),
       do: filtered(comment: "not annotated exit", id: @id, data: expected)

  @id 250
  @count 1
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_return_no_exit, {'lib/gen_event.ex', 460},
            {:no_return, [:only_normal, :system_terminate, 4]}}
       ),
       do: filtered(comment: "not annotated exit", id: @id, data: expected)

  yecc_erl_clauses =
    if System.otp_release() >= "24",
      do: [],
      else: [
        :yeccpars2_89,
        :yeccpars2_91,
        :yeccpars2_226,
        :yeccpars2_238,
        :yeccpars2_385,
        :yeccpars2_421,
        :yeccpars2_422
      ]

  @yecc_erl_clauses yecc_erl_clauses
  @yecc_yrl_functions [
                        :return_error,
                        :error_invalid_stab,
                        :error_bad_atom,
                        :error_no_parens_strict,
                        :error_no_parens_many_strict,
                        :error_no_parens_container_strict,
                        :error_invalid_kw_identifier,
                        :return_error_with_meta
                      ] ++
                        Enum.map(yecc_erl_clauses, fn a ->
                          String.to_atom(Atom.to_string(a) <> "_")
                        end)
  @id 260
  @count if System.otp_release() >= "24", do: 8, else: 15
  expected_counts = Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_return_no_exit, {'lib/elixir/src/elixir_parser.yrl', _},
            {:no_return, [:only_normal, fun_name, _arity]}}
       )
       when fun_name in @yecc_yrl_functions,
       do: filtered(comment: "parser not annotated exception", id: @id, data: expected)

  @id 265
  @count 6
  expected_counts =
    if System.otp_release() >= "24",
      do: Map.put(expected_counts, @id, 0),
      else: Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_return_no_exit, {'lib/elixir/src/elixir_parser.erl', _},
            {:no_return, [:only_normal, fun_name, _arity]}}
       )
       when fun_name in @yecc_erl_clauses,
       do: filtered(comment: "parser not annotated exception", id: @id, data: expected)

  @id 270
  @count 1
  expected_counts =
    if System.otp_release() >= "24",
      do: Map.put(expected_counts, @id, 0),
      else: Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_callgraph, {'lib/task.ex', _}, {:call_to_missing, [:erlang, :monitor, 3]}}
       ),
       do: filtered(comment: "run-time function_exported? check", id: @id, data: expected)

  @id 280
  @count 1
  expected_counts =
    if System.otp_release() >= "24",
      do: Map.put(expected_counts, @id, 0),
      else: Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_callgraph, {'lib/task/supervisor.ex', _},
            {:call_to_missing, [:erlang, :monitor, 3]}}
       ),
       do: filtered(comment: "run-time function_exported? check", id: @id, data: expected)

  @id 290
  @count 1
  expected_counts =
    if System.otp_release() >= "24",
      do: Map.put(expected_counts, @id, 0),
      else: Map.put(expected_counts, @id, @count)

  defp filter(
         expected = {:warn_callgraph, {'lib/system.ex', _}, {:call_to_missing, [:os, :env, 0]}}
       ),
       do: filtered(comment: "run-time function_exported? check", id: @id, data: expected)

  @id 300
  @count 1
  expected_counts =
    if System.otp_release() >= "24",
      do: Map.put(expected_counts, @id, 0),
      else: Map.put(expected_counts, @id, @count)

  defp filter(
         expected =
           {:warn_callgraph, {'lib/map.ex', _}, {:call_to_missing, [:maps, :from_keys, 2]}}
       ),
       do: filtered(comment: "run-time function_exported? check", id: @id, data: expected)

  defp filter(warning),
    do: unfiltered(data: warning)

  @expected_counts expected_counts
  defp expected_counts(), do: @expected_counts
end

Dialyzer.run()

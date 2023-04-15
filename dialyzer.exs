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
          "data:\n#{inspect(warning, limit: :infinity, printable_limit: :infinity, width: :infinity)}\n\n"

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
          "data:\n#{inspect(warning, limit: :infinity, printable_limit: :infinity, width: :infinity)}\n\n"
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

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)
  # discussed in https://github.com/elixir-lang/elixir/issues/10279
  # and https://github.com/elixir-lang/elixir/pull/10280
  # mostly fixed in https://github.com/elixir-lang/elixir/pull/10287
  # may be resolved in https://github.com/elixir-lang/elixir/issues/9465
  defp filter(
         dialyzer_warning =
           {:warn_failing_call, {'lib/logger.ex', 827},
            {:call, [Logger, :__do_log__, _, [3], :only_sig, _, _, {false, :none}]}}
       ),
       do:
         filtered(
           comment:
             "Elixir deliberately using erlang macro-based logger interface without passing in call location",
           id: @id,
           data: dialyzer_warning
         )

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  # discussed in https://github.com/elixir-lang/elixir/pull/9979#discussion_r416206411
  defp filter(
         dialyzer_warning =
           {:warn_matching, {'src/elixir_erl_compiler.erl', {59, _}},
            {:pattern_match, _lots_of_details}}
       ),
       do: filtered(comment: "return type not documented in erlang", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)
  # discussed in https://github.com/elixir-lang/elixir/issues/11092
  defp filter(
         dialyzer_warning =
           {:warn_matching, {'lib/calendar/time.ex', 718},
            {:pattern_match,
             [
               'pattern {\'error\', _reason@1}',
               '{\'ok\',\#{\'__struct__\':=\'Elixir.Time\', \'calendar\':=atom(), \'hour\':=non_neg_integer(), \'microsecond\':={non_neg_integer(),non_neg_integer()}, \'minute\':=non_neg_integer(), \'second\':=non_neg_integer()}}'
             ]}}
       ),
       do: filtered(comment: "slightly dead code", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
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
           data: dialyzer_warning
         )


  @id __ENV__.line
  expected_counts =
    if System.otp_release() < "26",
      do: Map.put(expected_counts, @id, 1),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         dialyzer_warning =
           {:warn_unknown, {~c"src/elixir.erl", _}, {:unknown_function, {:prim_tty, :isatty, 1}}}
       ),
       do: filtered(comment: "function used only conditionally on otp 26+", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts =
    if System.otp_release() < "26",
      do: Map.put(expected_counts, @id, 1),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         dialyzer_warning =
           {:warn_callgraph, {~c"lib/application.ex", _}, {:call_to_missing, [:application, :ensure_all_started, 3]}}
       ),
       do: filtered(comment: "function used only conditionally on otp 26+", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts =
    if System.otp_release() >= "26",
      do: Map.put(expected_counts, @id, 1),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
        dialyzer_warning =
          {:warn_unknown, {~c"lib/iex/cli.ex", _}, {:unknown_function, {:user, :start, 0}}}
      ),
      do: filtered(comment: "function used only conditionally on otp < 26", id: @id, data: dialyzer_warning)


  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 2)

  defp filter(
         dialyzer_warning =
           {:warn_unknown, {'lib/mix/hex.ex', lines}, {:unknown_function, {Hex, function, 0}}}
       )
       when function in [:start, :version] and lines in [41, 60],
       do:
         filtered(
           comment: "Hex package loading gets handled by the Mix task",
           id: @id,
           data: dialyzer_warning
         )

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_matching, {'lib/dynamic_supervisor.ex', 431},
            {:pattern_match_cov,
             [
               'variable _other@1',
               '{_,_,_,_,_,_} | \#{\'id\':=_, \'start\':={atom(),atom(),[any()]}, \'modules\'=>\'dynamic\' | [atom()], \'restart\'=>\'permanent\' | \'temporary\' | \'transient\', \'shutdown\'=>\'brutal_kill\' | \'infinity\' | non_neg_integer(), \'significant\'=>boolean(), \'type\'=>\'supervisor\' | \'worker\'}'
             ]}}
       ),
       do: filtered(comment: "overly defensive code", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_matching, {'lib/mix/utils.ex', 795},
            {:pattern_match,
             [
               'pattern \'nil\'',
               '\#{\'__struct__\':=\'Elixir.URI\', \'authority\':=\'Elixir.URI\':authority(), \'fragment\':=\'nil\' | binary(), \'host\':=\'nil\' | binary(), \'path\':=\'nil\' | binary(), \'port\':=\'nil\' | char(), \'query\':=\'nil\' | binary(), \'scheme\':=\'nil\' | binary(), \'userinfo\':=\'nil\' | binary()}'
             ]}}
       ),
       do: filtered(comment: "overly defensive code", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_matching, {'lib/iex/helpers.ex', 646},
            {:pattern_match,
             [
               'pattern <__key@1, \'nil\'>',
               '<<<_:64,_:_*8>>,binary() | string() | non_neg_integer()>'
             ]}}
       ),
       do: filtered(comment: "overly_defensive code", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts =
    if System.otp_release() < "26",
      do: Map.put(expected_counts, @id, 22),
      else: Map.put(expected_counts, @id, 0)

  defp filter(dialyzer_warning = {:warn_unknown, {file, 1}, {:unknown_function, {module, :__impl__, 1}}})
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
            ] and file in ['lib/collectable.ex', 'lib/enum.ex', 'lib/list/chars.ex'],
       do: filtered(comment: "some protocol consolidation stuff", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts =
    if System.otp_release() < "26",
      do: Map.put(expected_counts, @id, 6),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         dialyzer_warning =
           {:warn_unknown, {'lib/string/chars.ex', 3},
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
       do: filtered(comment: "some protocol consolidation stuff", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 5)

  defp filter(
         dialyzer_warning =
           {:warn_matching, {'lib/kernel.ex', line},
            {:pattern_match, ['pattern \'false\'', '\'true\'']}}
       )
       when line in [2062, 3573, 3966, 4036, 4423],
       do: filtered(comment: "inlined bootstrap check stuff", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 2)

  defp filter(dialyzer_warning = {:warn_not_called, {'lib/system.ex', _}, {:unused_fun, [function, 1]}})
       when function in [:read_stripped, :strip],
       do:
         filtered(
           comment: "functions called only during elixir compilation time",
           id: @id,
           data: dialyzer_warning
         )

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'lib/mix/tasks/test.ex', 608},
            {:no_return, [:only_normal, :raise_with_shell, 2]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'lib/mix/release.ex', _},
            {:no_return, [:only_normal, :bad_umbrella!, 0]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'lib/mix/dep/loader.ex', 254},
            {:no_return, [:only_normal, :invalid_dep_format, 1]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'lib/mix/scm/path.ex', 67},
            {:no_return, [:only_normal, :checkout, 1]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'lib/iex.ex', _}, {:no_return, [:only_normal, :__break__!, 2]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'src/elixir_quote.erl', _},
            {:no_return, [:only_normal, :bad_escape, 1]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'lib/mix/dep/converger.ex', 55},
            {:no_return, [:only_normal, :cycle_found, 1]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'lib/mix/tasks/iex.ex', 9},
            {:no_return, [:only_normal, :run, 1]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, 1),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'lib/config/provider.ex', 411},
            {:no_return, [:only_normal, :bad_path_abort, 2]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, 4),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'src/elixir_clauses.erl', location},
            {:no_return, [:only_normal]}}
       )
       when location in [{95, 43}, {114, 43}, {133, 16}, {220, 16}],
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, 2),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'src/elixir_erl_compiler.erl', location},
            {:no_return, [:only_normal]}}
       )
       when location in [{85, 21}, {87, 21}],
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, 1),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'src/elixir_erl_compiler.erl', {114, 1}},
            {:no_return, [:only_normal, :handle_file_error, 2]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, 1),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"src/elixir_erl.erl", {583, 1}},
           {:no_return, [:only_normal, :file_error, 2]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, 1),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'src/elixir_errors.erl', {184, 1}},
            {:no_return, [:only_normal, :raise_reserved, 4]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, 1),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'src/elixir_errors.erl', {190, 1}},
            {:no_return, [:only_normal, :raise_snippet, 5]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts =
    if System.otp_release() >= "25",
      do: Map.put(expected_counts, @id, 1),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'src/elixir_fn.erl', {121, 1}},
            {:no_return, [:only_normal, :invalid_capture, 3]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning = {:warn_return_no_exit, {'lib/iex/cli.ex', 161}, {:no_return, [:only_normal]}}
       ),
       do: filtered(comment: "not annotated exit", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'lib/kernel/cli.ex', 368},
            {:no_return, [:only_normal, :halt_standalone, 1]}}
       ),
       do: filtered(comment: "not annotated exit", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'lib/gen_event.ex', 460},
            {:no_return, [:only_normal, :system_terminate, 4]}}
       ),
       do: filtered(comment: "not annotated exit", id: @id, data: dialyzer_warning)

  @yecc_yrl_functions [
    :error_invalid_stab,
    :error_bad_atom,
    :error_no_parens_strict,
    :error_no_parens_many_strict,
    :error_no_parens_container_strict,
    :error_invalid_kw_identifier,
    :error_too_many_access_syntax,
    :return_error,
    :return_error_with_meta,
    :bad_keyword
  ]
  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 10)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {'lib/elixir/src/elixir_parser.yrl', _},
            {:no_return, [:only_normal, fun_name, _arity]}}
       )
       when fun_name in @yecc_yrl_functions,
       do: filtered(comment: "parser not annotated exception", id: @id, data: dialyzer_warning)

  defp filter(dialyzer_warning),
    do: unfiltered(data: dialyzer_warning)

  @expected_counts expected_counts
  defp expected_counts(), do: @expected_counts
end

Dialyzer.run()

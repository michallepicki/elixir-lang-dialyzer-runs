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
          ~c"releases",
          :erlang.system_info(:otp_release),
          ~c"OTP_VERSION"
        ])
      )
      |> String.trim()
      |> String.to_charlist()

    plt_filename = ~c"plt/" ++ otp_version ++ ~c".plt"

    dirs =
      [
        ~c"erts",
        ~c"kernel",
        ~c"stdlib",
        ~c"compiler",
        ~c"syntax_tools",
        ~c"parsetools",
        ~c"tools",
        ~c"ssl",
        ~c"inets",
        ~c"crypto",
        ~c"runtime_tools",
        ~c"ftp",
        ~c"tftp",
        ~c"mnesia",
        ~c"public_key",
        ~c"asn1",
        ~c"sasl"
      ]
      |> Enum.map(fn app ->
        :filename.join(:code.lib_dir(app), ~c"ebin")
      end)

    if !File.exists?(plt_filename) do
      IO.puts("Preparing #{plt_filename}")

      :dialyzer.run(
        analysis_type: :plt_build,
        output_plt: plt_filename,
        files_rec: dirs
      )
    end

    IO.puts("Starting dialyzer analysis.")

    results =
      :dialyzer.run(
        init_plt: plt_filename,
        warnings: [
          :overlapping_contract,
          :unknown,
          :no_improper_lists,
          :no_opaque
        ],
        files_rec: [
          ~c"elixir/lib/eex/ebin",
          ~c"elixir/lib/elixir/ebin",
          ~c"elixir/lib/ex_unit/ebin",
          ~c"elixir/lib/iex/ebin",
          ~c"elixir/lib/logger/ebin",
          ~c"elixir/lib/mix/ebin"
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
  # should have been but wasn't resolved in https://github.com/elixir-lang/elixir/issues/9465
  defp filter(
         dialyzer_warning =
           {:warn_failing_call, {~c"lib/logger.ex", {1007, 41}},
            {:call, [Logger, :__do_log__, _, [3], :only_sig, _, _, {false, :none}]}}
       ),
       do:
         filtered(
           comment: "Elixir deliberately using erlang macro-based logger interface without passing in call location",
           id: @id,
           data: dialyzer_warning
         )

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  # discussed in https://github.com/elixir-lang/elixir/pull/9979#discussion_r416206411
  defp filter(
         dialyzer_warning =
           {:warn_matching, {~c"src/elixir_erl_compiler.erl", {87, _}}, {:pattern_match, _lots_of_details}}
       ),
       do: filtered(comment: "return type not documented in erlang", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_matching, {~c"lib/calendar/date_range.ex", {209, 10}},
            {:pattern_match_cov,
             [
               ~c"pattern _date_range@1 = \#{'__struct__':='Elixir.Date.Range', 'first_in_iso_days':=_first_days@1, 'last_in_iso_days':=_last_days@1}",
               ~c"\#{'__struct__':='Elixir.Date.Range', 'first':=\#{'calendar':=_, _=>_}, 'first_in_iso_days':=_, 'last_in_iso_days':=_, 'step':=_, _=>_}"
             ]}}
       ),
       do:
         filtered(
           comment: "code added for backwards compatibility with old date ranges without step field",
           id: @id,
           data: dialyzer_warning
         )

  @id __ENV__.line
  expected_counts =
    if System.otp_release() < "27",
      do: Map.put(expected_counts, @id, 1),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         dialyzer_warning =
           {:warn_callgraph, {~c"lib/iex/config.ex", {43, 14}}, {:call_to_missing, [:shell, :prompt_width, 1]}}
       ),
       do: filtered(comment: "function used only conditionally on otp 27+", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts =
    if System.otp_release() > "26",
      do: Map.put(expected_counts, @id, 1),
      else: Map.put(expected_counts, @id, 0)

  defp filter(
         dialyzer_warning =
           {:warn_callgraph, {~c"lib/iex/config.ex", {41, 17}}, {:call_to_missing, [:prim_tty, :npwcwidthstring, 1]}}
       ),
       do: filtered(comment: "function used only conditionally on otp 26", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 2)

  defp filter(
         dialyzer_warning =
           {:warn_unknown, {~c"lib/mix/hex.ex", _}, {:unknown_function, {Hex, function, 0}}}
       )
       when function in [:start, :version],
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
           {:warn_unknown, {~c"lib/mix/tasks/local.hex.ex", _}, {:unknown_function, {Hex, :version, 0}}}
       ),
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
           {:warn_matching, {~c"lib/dynamic_supervisor.ex", {422, 8}},
            {:pattern_match_cov,
             [
               ~c"variable _other@1",
               ~c"{_,_,_,_,_,_} | \#{'id':=_, 'start':={atom(),atom(),[any()]}, 'modules'=>'dynamic' | [atom()], 'restart'=>'permanent' | 'temporary' | 'transient', 'shutdown'=>'brutal_kill' | 'infinity' | non_neg_integer(), 'significant'=>boolean(), 'type'=>'supervisor' | 'worker'}"
             ]}}
       ),
       do: filtered(comment: "overly defensive code", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_matching, {~c"lib/mix/utils.ex", {892, 8}},
            {:pattern_match,
             [
               ~c"pattern 'nil'",
               ~c"\#{'__struct__':='Elixir.URI', 'authority':='Elixir.URI':authority(), 'fragment':='nil' | binary(), 'host':='nil' | binary(), 'path':='nil' | binary(), 'port':='nil' | char(), 'query':='nil' | binary(), 'scheme':='nil' | binary(), 'userinfo':='nil' | binary()}"
             ]}}
       ),
       do: filtered(comment: "overly defensive code", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 6)

  defp filter(
         dialyzer_warning =
            {:warn_matching, {~c"lib/iex/info.ex", {_, 7}}, {:pattern_match_cov, [~c"variable _doc@2", ~c"{nonempty_maybe_improper_list() | {'doc_group',nonempty_maybe_improper_list(),'normal'},\#{'__struct__':='Elixir.Inspect.Opts', 'base':='binary' | 'decimal' | 'hex' | 'octal', 'binaries':='as_binaries' | 'as_strings' | 'infer', 'char_lists':=_, 'charlists':='as_charlists' | 'as_lists' | 'infer', 'custom_options':=[any()], 'inspect_fun':=fun((_,_) -> any()), 'limit':='infinity' | non_neg_integer(), 'pretty':=boolean(), 'printable_limit':='infinity' | non_neg_integer(), 'safe':=boolean(), 'structs':=boolean(), 'syntax_colors':=[any()], 'width':='infinity' | non_neg_integer()}}"]}}
       ),
       do: filtered(comment: "overly defensive code", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_matching, {~c"src/elixir_erl.erl", {531, 1}}, {:pattern_match, [~c"pattern <[], Opts>", ~c"<[any(),...],[any(),...]>"]}}
       ),
       do: filtered(comment: "currently redundant clause because at least the ExCk chunk will be present", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 5)

  defp filter(
         dialyzer_warning =
           {:warn_matching, {~c"lib/kernel.ex", location}, {:pattern_match, [~c"pattern 'false'", ~c"'true'"]}}
       )
       when location in [{2182, 15}, {3734, 17}, {4185, 13}, {4282, 13}, {4688, 15}],
       do: filtered(comment: "inlined bootstrap check stuff", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 2)

  defp filter(dialyzer_warning = {:warn_not_called, {~c"lib/system.ex", _}, {:unused_fun, [function, 1]}})
       when function in [:read_stripped, :strip],
       do:
         filtered(
           comment: "functions called only during elixir compilation time",
           id: @id,
           data: dialyzer_warning
         )

  @id __ENV__.line
  expected_counts =
    if System.otp_release() < "28",
      do: Map.put(expected_counts, @id, 0),
      else: Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_matching, {~c"lib/system.ex", {289, 42}}, {:exact_compare, [~c"<<_:56>>", :"/=", ~c"<<>>"]}}
       ),
       do: filtered(comment: "dead code warning caused by string literal baked in at compile time", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"lib/mix/tasks/test.ex", _}, {:no_return, [:only_normal, :raise_with_shell, 2]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"lib/mix/release.ex", _}, {:no_return, [:only_normal, :bad_umbrella!, 0]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"lib/mix/dep/loader.ex", _}, {:no_return, [:only_normal, :invalid_dep_format, 1]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"lib/mix/scm/path.ex", _}, {:no_return, [:only_normal, :checkout, 1]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"lib/iex.ex", _}, {:no_return, [:only_normal, :__break__!, 2]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"src/elixir_quote.erl", _}, {:no_return, [:only_normal, :bad_escape, 1]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"lib/mix/dep/converger.ex", _}, {:no_return, [:only_normal, :cycle_found, 1]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"lib/mix/tasks/iex.ex", _}, {:no_return, [:only_normal, :run, 1]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"lib/config/provider.ex", _}, {:no_return, [:only_normal, :bad_path_abort, 2]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 4)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"src/elixir_clauses.erl", location}, {:no_return, [:only_normal]}}
       )
       when location in [{247, 43}, {266, 43}, {285, 16}, {377, 16}],
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 2)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"src/elixir_erl_compiler.erl", _}, {:no_return, [:only_normal, function, _]}}
       )
       when function in [:handle_file_error, :incompatible_options],
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 2)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"src/elixir_erl_compiler.erl", location}, {:no_return, [:only_normal]}}
       )
       when location in [{122, 21}, {124, 21}],
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"src/elixir_erl.erl", _}, {:no_return, [:only_normal, :file_error, 2]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"src/elixir_errors.erl", _}, {:no_return, [:only_normal, :raise_reserved, 4]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"src/elixir_errors.erl", _}, {:no_return, [:only_normal, :raise_snippet, 5]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"src/elixir_fn.erl", _}, {:no_return, [:only_normal, :invalid_capture, 3]}}
       ),
       do: filtered(comment: "not annotated exception", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"lib/kernel/cli.ex", _}, {:no_return, [:only_normal, :halt_standalone, 1]}}
       ),
       do: filtered(comment: "not annotated exit", id: @id, data: dialyzer_warning)

  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"lib/gen_event.ex", _}, {:no_return, [:only_normal, :system_terminate, 4]}}
       ),
       do: filtered(comment: "not annotated exit", id: @id, data: dialyzer_warning)


  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"lib/mix/tasks/deps.partition.ex", {249, 11}}, {:no_return, [:only_normal]}}
       ),
       do: filtered(comment: "not annotated exit", id: @id, data: dialyzer_warning)


  @id __ENV__.line
  expected_counts = Map.put(expected_counts, @id, 1)

  defp filter(
         dialyzer_warning =
           {:warn_return_no_exit, {~c"lib/mix/tasks/deps.partition.ex", {191, 8}}, {:no_return, [:only_normal, :tcp_failed!, 4]}}
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
           {:warn_return_no_exit, {~c"lib/elixir/src/elixir_parser.yrl", _},
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

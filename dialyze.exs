defmodule Dialyzin do
  def filter([], acc) do
    acc
  end

  def filter([warning | rest], acc) do
    filter(rest, [filter_warning(warning) | acc])
  end

  defp filter_warning(
         warning = {:warn_unknown, {[], 0}, {:unknown_function, {module, :__impl__, 1}}}
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
              List.Chars.Tuple,
              String.Chars.Function,
              String.Chars.Map,
              String.Chars.PID,
              String.Chars.Port,
              String.Chars.Reference,
              String.Chars.Tuple
            ] do
    {:ok, "some protocol consolidation stuff", warning}
  end

  defp filter_warning(
         warning =
           {:warn_matching, {'lib/kernel.ex', _line},
            {:pattern_match, ['pattern \'false\'', '\'true\'']}}
       ) do
    {:ok, "inlined bootstrap check stuff", warning}
  end

  defp filter_warning(
         warning =
           {:warn_matching, {'src/elixir_erl_compiler.erl', 59},
            {:pattern_match, _lots_of_details}}
       ) do
    {:ok, "return type not documented in erlang", warning}
  end

  defp filter_warning(
         warning =
           {:warn_matching, {'lib/mix/tasks/compile.erlang.ex', 104},
            {:pattern_match, ['pattern {\'error\', \'badarg\'}', _]}}
       ) do
    {:ok, "return type not documented in erlang", warning}
  end

  defp filter_warning(
         warning =
           {:warn_failing_call, {'lib/logger.ex', _line},
            {:call,
             [
               :logger,
               :macro_log,
               [?(, ?\#, ?{, ?}, ?, | _],
               [1],
               :only_contract,
               _,
               _,
               _
             ]}}
       ) do
    {:ok,
     "Elixir deliberately using erlang macro-based logger interface without passing in call location",
     warning}
  end

  defp filter_warning(warning), do: {:warning, warning}
end

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
# |> IO.inspect(limit: :infinity, printable_limit: :infinity)
|> Dialyzin.filter([])
|> Enum.sort_by(fn
  {:warning, _} -> 1
  {:ok, _, _} -> 2
end)
|> Enum.map(fn
  {:warning, warning} ->
    formatted_warning =
      warning
      |> :dialyzer.format_warning(indent_opt: true)
      |> to_string()

    # "Potential issue: #{formatted_warning}\n"
    formatted_warning <> "\n"

  {:ok, _message, _} ->
    # "...something we can ignore (#{message})\n"
    nil
end)
|> Enum.filter(& &1)
|> Stream.into(File.stream!("report", [:write, :utf8]))
|> Stream.into(IO.stream(:stdio, :line))
|> Stream.run()

defmodule Rotor.GroupServer do
  use GenServer


  def start_link do
    Agent.start_link(fn-> %{} end, name: __MODULE__)
  end


  def all do
    Agent.get __MODULE__, &(&1)
  end


  def get(name) do
    Agent.get __MODULE__, &(get_in &1, [name])
  end


  def add(name, paths, rotor_fn, options \\ %{}) do
    paths = format_paths(paths)

    group_info = Agent.get_and_update __MODULE__, fn(groups)->
      group_info = build_group_info(paths, rotor_fn, options)
      updated_groups = put_in groups, [name], group_info
      {group_info, updated_groups}
    end

    start_file_watcher(name, group_info.options.manual)
    :ok
  end


  defp start_file_watcher(name, is_manual) do
    case Rotor.FileWatcherPool.add(name, is_manual) do
      {:error, {:already_started, _pid}} ->
        Rotor.FileWatcherPool.remove(name)
        start_file_watcher(name, is_manual)
      _ -> :ok
    end
  end


  def remove(name) do
    Agent.update __MODULE__, fn(groups)->
      Rotor.FileWatcherPool.remove(name)
      Map.delete groups, name
    end
  end


  defp set_default_options(options) do
    %{interval: 2500, manual: false} |> Map.merge(options)
  end


  defp build_group_info(paths, rotor_fn, options_passed) do
    options = set_default_options(options_passed)
    %{paths: paths, rotor_fn: rotor_fn, options: options}
  end


  defp format_paths(paths) do
    cond do
      String.valid?(paths) || :io_lib.char_list(paths) -> [paths]
      true -> paths
    end
  end
end

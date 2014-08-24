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

    pid = start_file_watcher(name, group_info.options)
    Agent.update __MODULE__, fn(groups)->
      put_in groups, [name, :file_watcher_pid], pid
    end
    :ok
  end


  def remove(name) do
    Agent.update __MODULE__, fn(groups)->
      Rotor.FileWatcherPool.remove(name)
      Map.delete groups, name
    end
  end


  def run(name) do
    group_info = Rotor.group_info name
    GenServer.call group_info.file_watcher_pid, :poll
  end


  def run_async(name) do
    group_info = Rotor.group_info name
    send group_info.file_watcher_pid, :poll
  end


  defp start_file_watcher(name, options) do
    case Rotor.FileWatcherPool.add(name, options.manual) do
      {:error, {:already_started, _pid}} ->
        Rotor.FileWatcherPool.remove(name)
        start_file_watcher(name, options)
      {:ok, pid} ->
        send(pid, :poll)
        pid
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

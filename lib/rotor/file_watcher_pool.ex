defmodule Rotor.FileWatcherPool do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end


  def init([]) do
    children = []
    supervise(children, strategy: :one_for_one)
  end


  def add(group_name, is_manual) do
    watcher_info = %{name: group_name, manual: is_manual}
    child = worker(Rotor.FileWatcher, [watcher_info], id: unique_id(group_name))
    Supervisor.start_child(__MODULE__, child)
  end


  def remove(group_name) do
    :ok = Supervisor.terminate_child __MODULE__, unique_id(group_name)
    :ok = Supervisor.delete_child __MODULE__, unique_id(group_name)
  end


  defp unique_id(group_name) do
    "#{group_name}-watcher"
  end
end

defmodule Rotor.FileWatcher do
  use GenServer
  import Rotor.Utils

  def add_group(name) do
    GenServer.call Rotor.FileWatcher, {:add, name}
  end


  def remove_group(name) do
    GenServer.call Rotor.FileWatcher, {:remove, name}
  end


  def init([]) do
    {:ok, []}
  end


  def handle_info({:trigger, name}, groups) do
    {:ok, group} = Rotor.WatchGroupServer.get_group(name)

    {changed_files, updated_index} = update_file_index_timestamps(group.file_index)
    :ok = Rotor.WatchGroupServer.update_file_index(name, updated_index)
    Rotor.EventServer.

    {:noreply, groups}
  end


  def handle_call({:add, name}, _from, groups) do
    updated_groups = if :lists.member(name, groups) do
      [name | groups]
    else
      groups
    end
    {:reply, :ok, updated_groups}
  end


  def handle_call({:remove, name}, _from, groups) do
    updated_groups = if :lists.member(name, groups) do
      :lists.delete(name, groups)
    else
      groups
    end
    {:reply, :ok, updated_groups}
  end


  Process.send_after(Rotor.Server, {:trigger, group_name, false}, 2500)
end

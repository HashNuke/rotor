defmodule Rotor.WatchGroupServer do
  use GenServer


  def start_link do
    GenServer.start(__MODULE__, [], name: __MODULE__)
  end


  def init([]) do
    {:ok, Map.new}
  end


  def handle_call(:groups, _from, state) do
    {:reply, state, state}
  end


  def handle_call({:group, name}, _from, state) do
    {:reply, get_in(state, [name]), state}
  end


  def handle_call({:add, name, paths, rotor_function, options}, _from, groups) do
    group = %{
      paths: paths,
      rotor_function: rotor_function,
      options: set_default_options(options),
      file_watcher_pid: start_file_watcher(name)
    }

    Process.link(group.file_watcher_pid)
    if group.options.interval != :manual do
      send(group.file_watcher_pid, :poll)
    end
    updated_groups = put_in groups[name], group
    {:reply, :ok, updated_groups}
  end


  def handle_call({:remove, name}, _from, groups) do
    file_watcher_pid = get_in groups, [name, :file_watcher_pid]
    if Process.alive?(file_watcher_pid) do
      Process.unlink(file_watcher_pid)
      Process.exit(file_watcher_pid, :kill)
    end
    updated_groups = Map.delete groups, name
    {:reply, :ok, updated_groups}
  end


  def handle_call({:poll, name}, groups) do
    group = get_in groups, [name]
    send(group.file_watcher_pid, :poll)
    {:reply, :ok, groups}
  end


  def handle_cast({:trigger, name, changed_files, all_files}, groups) do
    group = get_in groups, [name]

    try do
      apply group.rotor_function, [changed_files, all_files]
    rescue
      error ->
        IO.puts "Some problem running rotor function for group: #{name}"
        IO.inspect error
    end
    {:noreply, groups}
  end


  def trigger(name, changed_files, all_files) do
    GenServer.cast __MODULE__, {:trigger, name, changed_files, all_files}
  end


  defp set_default_options(options) do
    %{interval: 2500}
    |> Map.merge options
  end


  defp start_file_watcher(name) do
    {:ok, pid} = GenServer.start Rotor.FileWatcher, %{name: name}
    pid
  end

end

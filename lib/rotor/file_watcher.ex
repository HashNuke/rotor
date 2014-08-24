defmodule Rotor.FileWatcher do
  import Rotor.Utils
  use GenServer


  def start_link(args \\ []) do
    GenServer.start_link(__MODULE__, args)
  end


  def init(watcher_info) do
    {:ok, watcher_info}
  end


  def handle_call(:state, _from, state) do
    {:reply, state, state}
  end


  def handle_info(:poll, state) do
    group_info = Rotor.group_info(state.name)
    {changed_files, file_index} = case get_in(state, [:file_index]) do
      nil ->
        file_index = build_file_index(group_info.paths)
        state = put_in state[:file_index], file_index
        {[], file_index}
      index ->
        update_file_index_timestamps(index)
    end

    state = run_rotor_function(state, changed_files, file_index)
    schedule_poll(group_info.options.manual, group_info.options.interval)
    {:noreply, state}
  end


  defp schedule_poll(true, _interval) do
  end

  defp schedule_poll(false, interval) do
    Process.send_after(self, :poll, interval)
  end


  defp run_rotor_function(state, [], file_index) do
    state
  end

  defp run_rotor_function(state, changed_files, file_index) do
    state = put_in state[:file_index], file_index
    all_files = HashDict.values(file_index)
    Rotor.Runner.run(state.name, changed_files, all_files)
    state
  end
end

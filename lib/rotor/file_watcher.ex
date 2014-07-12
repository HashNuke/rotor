defmodule Rotor.FileWatcher do
  import Rotor.Utils
  use GenServer


  # group_info will have %{name: name}
  def init(group_info) do
    {:ok, group_info}
  end


  def handle_call(:state, _from, state) do
    {:reply, state, state}
  end


  def handle_info(:poll, state) do
    group_info = Rotor.group(state.name)
    {changed_files, file_index} = case get_in(state, [:file_index]) do
      nil ->
        file_index = build_file_index(group_info.paths)
        state = put_in state[:file_index], file_index
        {[], file_index}
      index ->
        update_file_index_timestamps(index)
    end

    if length(changed_files) != 0 do
      state = put_in state[:file_index], file_index
      Rotor.WatchGroupServer.trigger(state.name, changed_files, HashDict.values(file_index))
    end

    interval = get_in group_info, [:options, :interval]
    if interval != :manual do
      schedule_next_poll(interval)
    end
    {:noreply, state}
  end


  def schedule_next_poll(interval) do
    Process.send_after(self, :poll, interval)
  end
end

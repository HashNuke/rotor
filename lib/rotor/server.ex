defmodule Rotor.Server do
  use GenServer


  def handle_call({:add_group, group_name, group_config}, _from, state) do
    # This validates the format and keys
    file_index = build_file_index_without_duplicates(paths)
    group_config = Map.put_new group_config, :file_index, file_index
    run_rotor_function(group_name, group_config, true)

    #TODO centralized config in the Rotor.Server for time period
    timer_ref = Process.send_after(Rotor.Server, {:trigger, group_name, false}, 2500)
    group_config = Map.put_new group_config, :timer_ref, timer_ref

    new_group_list = Map.put state.groups, group_name, group_config

    new_state = %{state | :groups => new_group_list}
    {:reply, :ok, new_state}
  end


  def handle_call({:run, group_name, force_run_rotor_function}, _from, state) do
    {:ok, new_state} = trigger_group(group_name, state, force_run_rotor_function)
    {:reply, :ok, new_state}
  end


  def handle_call({:remove_group, name}, _from, state) do
    if Map.has_key?(state.groups, name) do
      :erlang.cancel_timer state.groups[name].timer_ref
      new_group_list = Map.delete(state.groups, name)
      new_state = %{state | :groups => new_group_list}
      {:reply, :ok, new_state}
    else
      {:reply, :no_such_group, state}
    end
  end


  def handle_info({:trigger, group_name, force_run_rotor_function}, state) do
    {:ok, new_state} = trigger_group(group_name, state, force_run_rotor_function)
    {:noreply, new_state}
  end


  def call(message) do
    GenServer.call Rotor.Server, message
  end


  defp trigger_group(group_name, current_state, force_run_rotor_function \\ false) do
    group_config = current_state.groups[group_name]
    {:ok, new_index} = run_rotor_function(group_name, group_config, force_run_rotor_function)

    updated_group = if force_run_rotor_function do
      Map.merge group_config, %{:file_index => new_index}
    else
      timer_ref = Process.send_after(Rotor.Server, {:trigger, group_name, false}, 2500)
      Map.merge group_config, %{:file_index => new_index, :timer_ref => timer_ref}
    end

    updated_groups = Map.put current_state.groups, group_name, updated_group
    new_state = Map.put current_state, :groups, updated_groups
    {:ok, new_state}
  end


  defp run_rotor_function(group_name, group_config, force_run_rotor_function) do
    [changed_files, new_index, is_index_changed] = update_file_index(group_config.file_index)
    IO.inspect "Listing changes..."
    IO.inspect changed_files
    IO.inspect "end of changes"

    if force_run_rotor_function || is_index_changed do
      IO.inspect "RUNNING: #{group_name}"
      apply group_config.rotor_function, [changed_files, HashDict.values(new_index)]
      IO.inspect "COMPLETED: #{group_name}"
    end

    {:ok, new_index}
  end


  defp update_file_index(current_index) do
    reducer = fn({path, file}, [changed_files, index, is_changed])->
      {:ok, stat} = File.stat(path)

      if file.last_modified_at != stat.mtime do
        file = %{file | :last_modified_at => stat.mtime}
        new_index = HashDict.put_new(index, path, file)
        [
          changed_files ++ [file],
          new_index,
          true
        ]
      else
        [changed_files, index, is_changed]
      end
    end

    Enum.reduce(current_index, [[], current_index, false], reducer)
  end


end

defmodule Rotor.Server do
  use GenServer


  def start_link do
    GenServer.start(__MODULE__, [], name: Rotor.Server)
  end


  def init([]) do
    state = %{:groups => %{}}
    {:ok, state}
  end


  def handle_call(:current_state, _from, state) do
    {:reply, state, state}
  end


  def handle_call([:add_group, group_name, group_config], _from, state) do
    # This validates the format and keys
    %{:paths => paths, :rotor => rotor} = group_config
    file_index = build_file_index_without_duplicates(paths)
    group_config = Map.put_new group_config, :file_index, file_index
    run_rotor(group_name, group_config, true)

    #TODO centralized config in the Rotor.Server for time period
    timer_ref = Process.send_after(Rotor.Server, [:trigger, group_name, false], 2500)
    group_config = Map.put_new group_config, :timer_ref, timer_ref

    new_group_list = Map.update state.groups, group_name, group_config, fn(value)->
      group_config
    end

    new_state = %{state | :groups => new_group_list}
    {:reply, :ok, new_state}
  end


  def handle_call([:run, group_name, force_run_rotor], _from, state) do
    {:ok, new_state} = trigger_group(group_name, state, force_run_rotor)
    {:reply, :ok, new_state}
  end


  def handle_call([:remove_group, name], _from, state) do
    if Map.has_key?(state.groups, name) do
      :erlang.cancel_timer state.groups[name].timer_ref
      new_group_list = Map.delete(state.groups, name)
      new_state = %{state | :groups => new_group_list}
      {:reply, :ok, new_state}
    else
      {:reply, :no_such_group, state}
    end
  end


  def handle_info([:trigger, group_name, force_run_rotor], state) do
    {:ok, new_state} = trigger_group(group_name, state, force_run_rotor)
    {:noreply, state}
  end


  def call(message) do
    GenServer.call Rotor.Server, message
  end


  defp trigger_group(group_name, current_state, force_run_rotor \\ false) do
    group_config = current_state.groups[group_name]
    {:ok, new_index} = run_rotor(group_name, group_config, force_run_rotor)

    updated_group = if force_run_rotor do
      Map.merge group_config, %{:file_index => new_index}
    else
      timer_ref = Process.send_after(Rotor.Server, [:trigger, group_name, false], 2500)
      Map.merge group_config, %{:file_index => new_index, :timer_ref => timer_ref}
    end

    new_state = Map.update current_state, group_name, updated_group, fn(_val)->
      updated_group
    end
    {:ok, new_state}
  end


  defp run_rotor(group_name, group_config, force_trigger_rotor) do
    [new_index, is_index_changed] = update_file_index(group_config.file_index)

    if force_trigger_rotor || is_index_changed do
      IO.inspect "RUNNING: #{group_name}"
      apply group_config.rotor, [HashDict.values(new_index)]
      IO.inspect "COMPLETED: #{group_name}"
    end

    {:ok, new_index}
  end


  defp update_file_index(current_index) do
    reducer = fn({path, file}, [index, is_changed])->
      {:ok, stat} = File.stat(path)

      if file.last_modified_at != stat.mtime do
        new_index = HashDict.update! index, path, fn(_value)->
          %{file | :last_modified_at => stat.mtime}
        end
        [new_index, true]
      else
        [index, is_changed]
      end
    end

    Enum.reduce(current_index, [current_index, false], reducer)
  end


  defp build_file_index_without_duplicates([], file_index) do
    file_index
  end


  defp build_file_index_without_duplicates([path | paths], file_index \\ HashDict.new()) do
    updated_file_index = Enum.reduce Path.wildcard(path), file_index, fn(file_path, index)->
      {:ok, file_info} = File.stat(file_path)
      if file_info.type == :directory || HashDict.has_key?(index, file_path) do
        index
      else
        file_props = %{:path => file_path, :last_modified_at => file_info.mtime}
        HashDict.put_new index, file_path, file_props
      end
    end

    build_file_index_without_duplicates(paths, updated_file_index)
  end

end

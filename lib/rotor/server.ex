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
    %{:paths => paths, :pipeline => pipeline} = group_config
    file_index = build_file_index_without_duplicates(paths)
    group_data = Map.put_new group_config, :file_index, file_index

    new_group_list = Map.update state.groups, group_name, group_data, fn(value)->
      group_data
    end

    run(pipeline, file_index, true)
    new_state = %{state | :groups => new_group_list}
    {:reply, :ok, new_state}
  end


  def handle_call([:run, group_name, force_trigger_pipeline], _from, state) do
    group = state.groups[group_name]
    {:ok, new_index} = run(group.pipeline, group.file_index, force_trigger_pipeline)
    updated_group = %{group | :file_index => new_index}

    new_state = Map.update state, group_name, updated_group, fn(_val)->
      updated_group
    end

    {:reply, :ok, new_state}
  end


  def handle_call([:remove_group, name], _from, state) do
    if Map.has_key?(state.groups, name) do
      new_group_list = Map.delete(state.groups, name)
      new_state = %{state | :groups => new_group_list}
      {:reply, :ok, new_state}
    else
      {:reply, :no_such_group, state}
    end
  end


  def call(message) do
    GenServer.call Rotor.Server, message
  end


  defp run(pipeline, current_index, force_trigger_pipeline) do
    [new_index, is_index_changed] = update_file_index(current_index)

    if force_trigger_pipeline || is_index_changed do
      apply pipeline, [HashDict.values(new_index)]
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

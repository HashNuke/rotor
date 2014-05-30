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

    files = list_files_without_duplicates(paths)
    group_data = Map.put_new group_config, :file_index, files

    new_group_list = Map.update state.groups, group_name, group_data, fn(value)->
      group_data
    end

    new_state = %{state | :groups => new_group_list}
    {:reply, :ok, new_state}
  end


  def handle_call([:run, group_name], _from, state) do
    group = state.groups[group_name]
    file_index = group.file_index

    # NOTE dont stop when trigger_pipeline is true,
    # because timestamps for other files are needed
    reducer = fn({path, file}, [index, trigger_pipeline])->
      {:ok, stat} = File.stat(path)

      if file.last_modified_at != stat.mtime do
        updated_index = HashDict.update! index, path, fn(_value)->
          %{file | :last_modified_at => stat.mtime}
        end
        [updated_index, true]
      else
        [index, trigger_pipeline]
      end
    end


    [new_index, trigger_pipeline] = Enum.reduce(file_index, [file_index, false], reducer)
    updated_group = %{group | :file_index => new_index}
    new_state = Map.update state, group_name, updated_group, fn(_val)->
      updated_group
    end

    if trigger_pipeline do
      apply group.pipeline, [HashDict.keys(new_index)]
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


  defp list_files_without_duplicates([], file_index) do
    file_index
  end


  defp list_files_without_duplicates([path | paths], file_index \\ HashDict.new()) do
    updated_file_index = Enum.reduce Path.wildcard(path), file_index, fn(file_path, index)->
      {:ok, file_info} = File.stat(file_path)
      if file_info.type == :directory || HashDict.has_key?(index, file_path) do
        index
      else
        file_props = %{:path => file_path, :last_modified_at => file_info.mtime}
        HashDict.put_new index, file_path, file_props
      end
    end

    list_files_without_duplicates(paths, updated_file_index)
  end

end

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
    group_data = %{group_config | :file_index => files}

    new_group_list = Map.update state.groups, group_name, group_data, fn(value)->
      value
    end

    new_state = %{state | :groups => new_group_list}
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


  defp list_files_without_duplicates(paths) do
    List.foldl paths, HashDict.new(), fn(path, files)->
      Enum.each Path.wildcard(path), fn(file_path)->
        {:ok, file_info} = File.stat(file_path)
        if file_info.type == :directory || HashDict.has_key?(files, file_path) do
          files
        else
          file_props = %{:path => file_path, :last_modified_at => file_info.mtime}
          HashDict.put_new files, file_path, file_props
        end
      end
    end
  end

end

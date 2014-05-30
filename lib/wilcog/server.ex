defmodule Wilcog.Server do
  use GenServer

  def init(_args) do
    state = %{:groups => %{}, :files => HashDict.new()}
    {:ok, state}
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


  def handle_call([:refresh, group_name], _from, state) do
    file_list = list_files_without_duplicates(state.groups[group_name].paths)

    group = state.groups[group_name]
    updated_group = %{group | :files => file_list}

    new_group_list = Map.update! state.groups, group_name, updated_group, fn(value)->
      value
    end

    new_state = %{state | :groups => new_group_list}
    {:reply, :ok, new_state}
  end


  defp list_files_without_duplicates(paths)
    List.foldl paths, [], fn(path, file_paths)->
      Enum.each Path.wildcard(path), fn(file_path)->
        if Enum.member?(file_paths, file_path) do
          file_paths
        else
          [file_path | file_paths]
        end
      end
    end
  end

  # def handle_call([:remove_group, name], _from, state) do
  #   if Map.has_key?(state.groups, name) do
  #     new_group_list = Map.delete(state.groups, name)
  #     new_state = %{state | :groups => new_group_list}
  #     {:reply, :ok, new_state}
  #   else
  #     {:reply, :no_such_group, state}
  #   end
  # end


  defp index_files_for_group(group, paths, current_index) do
    Enum.foldl Path.wildcard(path), current_index, fn(file_path, index)->
      HashDict.update index, file_path, %{:groups => [group]}, fn(file_props)->
        if Enum.member?(file_props.groups, group) do
          file_props
        else
          %{file_props | :groups => [group | file_props.groups]}
        end
      end
    end
  end


  def call(message) do

  end
end

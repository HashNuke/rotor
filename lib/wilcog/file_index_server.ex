defmodule Wilcog.FileIndexServer do
  use GenServer

  def init(_args) do
    state = HashDict.new()
    {:ok, state}
  end


  def handle_call([:add_paths, group, paths], _from, state) do
    new_index = index_files_for_group(group, paths, current_index)
    {:reply, :ok, new_index}
  end


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

end

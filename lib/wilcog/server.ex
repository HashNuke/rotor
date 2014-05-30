defmodule Wilcog.Server do
  use GenServer

  def init(_args) do
    state = %{:groups => %{}, :files => HashDict.new()}
    {:ok, state}
  end


  def handle_call([:add_group, name, config], _from, state) do
    # This line validates the format and keys
    %{:paths => paths, :pipeline => pipeline} = config

    new_group_list = if Map.has_key?(state.groups, name) do
      %{state.groups | name => config}
    else
      Map.put_new state.groups, name, config
    end

    new_state = %{state | :groups => new_group_list}
    {:reply, :ok, new_state}
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


  def register_path_for_group(path, group) do
    files = state.files
    Enum.each :filelib.wildcard('#{path}'), fn(file_path)->
      HashDict.update(files, file_path, %{:groups => [group]}, fn(file_props)->
        if Enum.member? file_props.groups, group do
          file_props
        else
          %{file_props | :groups => [group | file_props.groups]}
        end
      end
    end
    new_state = %{state | :files => files}
  end


end

defmodule Rotor.WatchGroupServer do

  def start_link do
    Agent.start_link(fn -> Map.new end, name: __MODULE__)
  end


  def add_group(name, paths, file_index, rotor_function, options \\ %{}) do
    {:ok, default_limit} = Rotor.ConfigServer.get(:parallel_limit)
    limit = Map.get(options, :parallel_limit, default_limit)

    Agent.get_and_update __MODULE__, fn(groups)->
      group = %{
        :paths => paths,
        :file_index => file_index,
        :rotor_function => rotor_function,
        :parallel_limit => limit
      }

      updated_groups = Map.put groups, name, group
      :ok = Rotor.FileWatcher.add_group(name)
      {:ok, updated_groups}
    end
  end


  def get_group(name) do
    Agent.get __MODULE__, fn(state)->
      {:ok, state[name]}
    end
  end


  def update_file_index(name, new_index) do
    Agent.get_and_update __MODULE__,  fn(groups)->
      group = %{groups[name] | :file_index => new_index}
      updated_groups = Map.put groups, name, group
      {:ok, updated_groups}
    end
  end


  def remove_group(name) do
    Agent.get_and_update __MODULE__,  fn(groups)->
      updated_groups = Map.delete groups, name
      {:ok, updated_groups}
    end
  end

end

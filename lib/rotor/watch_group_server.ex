defmodule Rotor.WatchGroupServer do

  def start_link do
    Agent.start_link(fn -> Map.new end, name: __MODULE__)
  end


  def group(name) do
    Agent.get __MODULE__, fn(state)->
      {:ok, state[name]}
    end
  end


  def add(name, paths, rotor_function, options \\ %{}) do
    Agent.get_and_update __MODULE__, fn(groups)->
      group = %{
        paths: paths,
        rotor_function: rotor_function,
        options: options
      }

      updated_groups = put_in groups[name], group
      :ok = Rotor.FileWatcher.add_group(name)
      {:ok, updated_groups}
    end
  end


  def remove(name) do
    Agent.get_and_update __MODULE__,  fn(groups)->
      updated_groups = Map.delete groups, name
      {:ok, updated_groups}
    end
  end


  def trigger(name, changed_files, all_files) do
    Agent.get __MODULE__, fn(state)->
      group = get_in state, [name]

      try do
        apply group.rotor_function, [changed_files, all_files]
      rescue
        _ ->
          IO.puts "Some problem running rotor function for group: #{name}"
      end
    end
  end

end

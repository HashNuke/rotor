defmodule Rotor.ConfigServer do
  def start_link do
    Agent.start_link(fn -> %{:parallel_limit => 1} end, name: __MODULE__)
  end


  def set(option, value) do
    Agent.get_and_update __MODULE__, fn(config)->
      updated_config = Map.put(config, option, value)
      {:ok, updated_config}
    end
  end


  def get(option) do
    Agent.get __MODULE__, fn(config)->
      Map.get(config, option)
    end
  end
end

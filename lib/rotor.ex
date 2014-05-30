defmodule Rotor do
  use Application

  # See http://elixir-lang.org/docs/stable/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      # Define workers and child supervisors to be supervised
      worker(Rotor.Server, [])
    ]

    # See http://elixir-lang.org/docs/stable/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Rotor.Supervisor]
    Supervisor.start_link(children, opts)
  end


  def add_group(group_name, paths, pipeline) do
    group_props = %{:paths => format_paths(paths), :pipeline => pipeline}
    Rotor.Server.call [:add_group, group_name, group_props]
  end


  def remove_group(group_name) do
    Rotor.Server.call [:remove_group, group_name]
  end


  def groups do
    current_state = Rotor.Server.call :current_state
    Map.keys current_state.groups
  end


  def run(group_name) do
    Rotor.Server.call [:run, group_name, true]
  end


  defp format_paths(paths) do
    cond do
      :io_lib.char_list(paths) -> ["#{paths}"]
      is_binary(paths) -> [paths]
      true -> paths
    end
    |> Enum.map(fn(path)-> "#{path}" end)
  end
end

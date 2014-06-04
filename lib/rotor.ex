defmodule Rotor do
  use Application

  import Rotor.Utils

  # See http://elixir-lang.org/docs/stable/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      # Define workers and child supervisors to be supervised
      worker(Rotor.ConfigServer, []),
      worker(Rotor.WatchGroupServer, []),
      worker(Rotor.Server, [])
    ]

    # See http://elixir-lang.org/docs/stable/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Rotor.Supervisor]
    Supervisor.start_link(children, opts)
  end


  def watch(name, paths, rotor_function) do
    file_index = format_paths(paths)
    |> build_file_index
    :ok = Rotor.WatchGroupServer.add_group(name, paths, file_index, rotor_function)
  end


  def stop_watching(name) do
    #TODO remove timers
    Rotor.WatchGroupServer.remove_group name
  end


  # def groups do
  #   current_state = Rotor.Server.call :current_state
  #   Map.keys current_state.groups
  # end


  def run(group_name) do
    Rotor.EventServer.trigger group_name
  end


  defp format_paths(paths) do
    cond do
      String.valid?(paths) || :io_lib.char_list(paths) -> [paths]
      true -> paths
    end
  end

end

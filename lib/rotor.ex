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
      worker(Rotor.WatchGroupServer, [])
    ]

    # See http://elixir-lang.org/docs/stable/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Rotor.Supervisor]
    Supervisor.start_link(children, opts)
  end


  def watch(name, paths, rotor_function, options \\ %{}) do
    paths = format_paths(paths)
    :ok = Rotor.WatchGroupServer.add(name, paths, rotor_function, options)
  end


  def stop_watching(name) do
    Rotor.WatchGroupServer.remove(name)
  end


  def groups do
    Rotor.WatchGroupServer.groups
  end


  defp format_paths(paths) do
    cond do
      String.valid?(paths) || :io_lib.char_list(paths) -> [paths]
      true -> paths
    end
  end

end

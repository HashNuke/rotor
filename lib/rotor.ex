defmodule Rotor do
  use Application

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


  def group(name) do
    GenServer.call Rotor.WatchGroupServer, {:group, name}
  end


  def watch(name, paths, rotor_function, options \\ %{}) do
    paths = format_paths(paths)
    GenServer.call Rotor.WatchGroupServer, {:add, name, paths, rotor_function, options}
  end


  def stop_watching(name) do
    GenServer.call Rotor.WatchGroupServer, {:remove, name}
  end


  def groups do
    GenServer.call Rotor.WatchGroupServer, :groups
  end


  def run(name) do
    group_info = Rotor.group(name)
    all_files = Rotor.Utils.build_file_index(group_info.paths)
                |> HashDict.values
    Rotor.WatchGroupServer.trigger(name, all_files, all_files)
  end


  defp format_paths(paths) do
    cond do
      String.valid?(paths) || :io_lib.char_list(paths) -> [paths]
      true -> paths
    end
  end

end

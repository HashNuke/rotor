defmodule Wilcog do
  use Application

  # See http://elixir-lang.org/docs/stable/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      # Define workers and child supervisors to be supervised
      worker(Wilcog.Server, [])
    ]

    # See http://elixir-lang.org/docs/stable/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Wilcog.Supervisor]
    Supervisor.start_link(children, opts)
  end


  def add_group(group_name, paths) do
    Wilcog.Server.call [:add_group, group_name, format_paths(paths)]
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

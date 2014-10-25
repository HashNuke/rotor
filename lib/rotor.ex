defmodule Rotor do
  use Application

  def start(_type, _args) do

    # If the supervisor is started correctly,
    # load rotors from the default rotors file
    case Rotor.Supervisor.start_link do
      {:ok, _pid} = result ->
        load_rotors
        result
      anything -> anything
    end
  end


  defdelegate group_info(name), to: Rotor.GroupServer, as: :get
  defdelegate all_groups(),     to: Rotor.GroupServer, as: :all
  defdelegate watch(name, paths, rotor_fn, options), to: Rotor.GroupServer, as: :add
  defdelegate watch(name, paths, rotor_fn),          to: Rotor.GroupServer, as: :add


  defdelegate run(name),       to: Rotor.GroupServer
  defdelegate run_async(name), to: Rotor.GroupServer


  defdelegate remove_group(name),  to: Rotor.GroupServer, as: :remove
  defdelegate stop_watching(name), to: Rotor.GroupServer, as: :remove


  def default_rotors_path do
    "config/rotors.exs"
  end


  def load_rotors do
    load_rotors default_rotors_path
  end


  def load_rotors(path) do
    if File.exists?(path) do
      Code.load_file path
    end
  end
end

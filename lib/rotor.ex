defmodule Rotor do
  use Application

  def start(_type, _args) do
    Rotor.Supervisor.start_link
  end


  defdelegate group_info(name), to: Rotor.GroupServer, as: :get
  defdelegate all_groups(), to: Rotor.GroupServer, as: :all
  defdelegate watch(name, paths, rotor_fn, options), to: Rotor.GroupServer, as: :add
  defdelegate watch(name, paths, rotor_fn), to: Rotor.GroupServer, as: :add


  defdelegate remove_group(name),  to: Rotor.GroupServer, as: :remove
  defdelegate stop_watching(name), to: Rotor.GroupServer, as: :remove

end

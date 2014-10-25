defmodule Rotor.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    children = [
      worker(Rotor.ConfigServer, []),
      worker(Rotor.GroupServer, []),
      supervisor(Rotor.FileWatcherPool, [])
    ]


    # If the supervisor is started correctly,
    # load rotors from the default rotors file
    case supervise(children, strategy: :one_for_one) do
      {:ok, something} = result ->
        Rotor.load_rotors
        result
      _anything = result ->
        result
    end
  end
end

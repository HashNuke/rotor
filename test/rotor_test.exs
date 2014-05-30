defmodule RotorTest do
  use ExUnit.Case

  test "state should be initialized on server start" do
    current_state = Rotor.Server.call(:current_state)
    assert %{:groups => %{}} == current_state
  end
end

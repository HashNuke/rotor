defmodule RotorTest do
  use ExUnit.Case

  import Rotor.Actions

  test "state should be initialized on server start" do
    current_state = Rotor.Server.call(:current_state)
    assert %{:groups => %{}} == current_state
  end


  test "should be able to add and remove groups" do
    output_path = "test/samples/outputs/app.js"
    Rotor.add_group :javascripts, ["test/samples/*.js"], fn(files)->
      read_files(files)
      |> concat
      |> output_to(output_path)
    end

    current_state = Rotor.Server.call(:current_state)
    assert current_state.groups[:javascripts] != nil

    Rotor.remove_group(:javascripts)
    current_state = Rotor.Server.call(:current_state)
    assert %{:groups => %{}} == current_state
  end


  test "should watch for changes and run pipeline functions" do
    output_path = "test/samples/outputs/app.js"
    Rotor.add_group :javascripts, ["test/samples/*.js"], fn(files)->
      read_files(files)
      |> concat
      |> output_to(output_path)
    end

    :ok = :timer.sleep(3000)

    {:ok, contents} = File.read output_path
    assert Regex.match?(~r/x=1/, contents) && Regex.match?(~r/y=2/, contents)

    Rotor.remove_group(:javascripts)
  end
end

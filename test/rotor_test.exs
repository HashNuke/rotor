defmodule RotorTest do
  use ExUnit.Case

  import Rotor.BasicRotors

  test "state should be initialized on server start" do
    current_state = Rotor.groups
    assert %{} == current_state
  end


  test "should be able to add and remove groups" do
    output_path = "test/samples/outputs/app.js"
    Rotor.watch :javascripts, ["test/samples/*.js"], fn(_changed_files, all_files)->
      read_files(all_files)
      |> concat
      |> output_to(output_path)
    end

    current_state = Rotor.groups
    assert get_in(current_state, [:javascripts]) != nil

    Rotor.stop_watching(:javascripts)
    current_state = Rotor.groups
    assert %{} == current_state
  end


  test "should watch for changes and run pipeline functions" do
    output_path = "test/samples/outputs/app.js"
    Rotor.watch :javascripts, ["test/samples/*.js"], fn(_changed_files, all_files)->
      read_files(all_files)
      |> concat
      |> output_to(output_path)
    end

    :ok = :timer.sleep(3000)

    {:ok, contents} = File.read output_path
    assert Regex.match?(~r/x=1/, contents) && Regex.match?(~r/y=2/, contents)

    Rotor.stop_watching(:javascripts)
  end
end

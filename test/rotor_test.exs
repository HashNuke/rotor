defmodule RotorTest do
  use ExUnit.Case

  import Rotor.BasicRotors

  test "state should be initialized on server start" do
    current_state = Rotor.all_groups
    assert is_map(current_state)
  end


  test "should be able to add and remove groups" do
    output_path = "test/samples/outputs/app.js"
    Rotor.watch :javascripts, ["test/samples/*.js"], fn(_changed_files, all_files)->
      read_files(all_files)
      |> concat
      |> output_to(output_path)
    end

    current_state = Rotor.all_groups
    assert get_in(current_state, [:javascripts]) != nil

    Rotor.remove_group(:javascripts)
    current_state = Rotor.all_groups
    assert Map.has_key?(current_state, :javascripts) == false
  end


  test "should watch for changes and run pipeline functions" do
    output_path = "test/samples/outputs/app.js"
    Rotor.watch :javascripts_pipeline_test, ["test/samples/*.js"], fn(_changed_files, all_files)->
      IO.inspect "Running callback for javascripts"
      read_files(all_files)
      |> concat
      |> output_to(output_path)
    end

    :ok = File.touch "test/samples/app1.js"
    :ok = :timer.sleep(3000)
    Rotor.stop_watching(:javascripts_pipeline_test)

    {:ok, contents} = File.read output_path
    assert Regex.match?(~r/x=1/, contents) && Regex.match?(~r/y=2/, contents)
  end
end

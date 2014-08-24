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
    File.rm output_path
    Rotor.watch :javascripts_pipeline_test, ["test/samples/*.js"], fn(_changed_files, all_files)->
      read_files(all_files)
      |> concat
      |> output_to(output_path)
    end

    :ok = :timer.sleep(1000)
    :ok = File.touch "test/samples/app1.js"
    :ok = :timer.sleep(2000)
    Rotor.stop_watching(:javascripts_pipeline_test)

    {:ok, contents} = File.read output_path
    assert Regex.match?(~r/x=1/, contents) && Regex.match?(~r/y=2/, contents)
  end


  test "should not watch for changes if group is set to manual" do
    group_name = :javascripts_pipeline_test
    output_path = "test/samples/outputs/app.js"
    File.rm output_path

    Rotor.watch(group_name, ["test/samples/*.js"], fn(_changed_files, all_files)->
      read_files(all_files)
      |> concat
      |> output_to(output_path)
    end, %{manual: true})

    # Touch the file
    :ok = :timer.sleep(1000)
    :ok = File.touch "test/samples/app1.js"
    :ok = :timer.sleep(2000)

    # Should be an error
    assert File.read(output_path) == {:error, :enoent}

    Rotor.run(group_name)
    Rotor.stop_watching(group_name)

    {:ok, contents} = File.read output_path
    assert Regex.match?(~r/x=1/, contents) && Regex.match?(~r/y=2/, contents)
  end


  test "should not watch for changes if group is set to manual (async)" do
    group_name = :javascripts_pipeline_test
    output_path = "test/samples/outputs/app.js"
    File.rm output_path

    Rotor.watch(group_name, ["test/samples/*.js"], fn(_changed_files, all_files)->
      read_files(all_files)
      |> concat
      |> output_to(output_path)
    end, %{manual: true})

    # Touch the file
    :ok = :timer.sleep(1000)
    :ok = File.touch "test/samples/app1.js"
    :ok = :timer.sleep(2000)

    # Should be an error
    assert File.read(output_path) == {:error, :enoent}

    Rotor.run_async(group_name)
    :ok = :timer.sleep(1000)
    Rotor.stop_watching(group_name)

    {:ok, contents} = File.read output_path
    assert Regex.match?(~r/x=1/, contents) && Regex.match?(~r/y=2/, contents)
  end
end

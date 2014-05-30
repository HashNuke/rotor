defmodule RotorHelpersTest do
  use ExUnit.Case
  import Rotor.Helpers


  def sample_files do
    [
      %{:path => "test1.txt", :contents => "john"},
      %{:path => "test2.txt", :contents => "doe"},
    ]
  end


  test "should read files" do
    files = ["test/samples/test1.txt", "test/samples/test2.txt"]
    files = read_files(files)

    assert hd(files).contents == "john\n"
    assert List.last(files).contents == "doe\n"
  end


  test "should concat files and return a string" do
    assert concat(sample_files) == "john\ndoe"
  end


  test "should content write to the file path specified" do
    output_path = "test/samples/outputs/output_test.txt"
    if File.exists?(output_path) do
      File.rm output_path
    end

    concat(sample_files)
    |> output_to(output_path)

    {:ok, output} = File.read output_path

    assert output == "john\ndoe"
  end
end

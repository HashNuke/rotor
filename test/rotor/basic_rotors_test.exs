defmodule BasicRotorsTest do
  use ExUnit.Case
  import Rotor.BasicRotors


  def sample_files do
    [
      %{:path => "test/samples/test1.txt", :contents => nil},
      %{:path => "test/samples/test2.txt", :contents => nil},
    ]
  end


  test "should read files" do
    files = read_files(sample_files)

    assert hd(files).contents == "john\n"
    assert List.last(files).contents == "doe\n"
  end


  test "should copy files" do
    file1 = "test/samples/outputs/test1.txt"
    file2 = "test/samples/outputs/test2.txt"
    File.rm(file1) && File.rm(file2)

    copy_files sample_files, "test/samples/outputs"
    assert File.exists?(file1) && File.exists?(file2)
  end


  test "should concat files and return a string" do
    files = read_files(sample_files)
    assert concat(files) == "john\n\ndoe\n"
  end


  test "should content write to the file path specified" do
    output_path = "test/samples/outputs/output_test.txt"
    if File.exists?(output_path) do
      File.rm output_path
    end

    read_files(sample_files)
    |> concat
    |> output_to(output_path)

    {:ok, output} = File.read output_path
    assert output == "john\n\ndoe\n"
  end
end

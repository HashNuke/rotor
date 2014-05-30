defmodule RotorHelpersTest do
  use ExUnit.Case

  test "should concatenate files" do
    assert 1 + 1 == 2
  end


  test "should concat files and return a string" do
    files = [
      %{:path => "test1.txt", :contents => "john"},
      %{:path => "test2.txt", :contents => "doe"},
    ]

    assert Rotor.Helpers.concat(files) == "john\ndoe"
  end


  test "" do
  end
end

defmodule Rotor.Helpers do


  def concat(files) do
    Enum.map_join(files, "\n", fn(file)-> file.contents end)
  end


  def write_to(contents, output_path) do
    File.write output_path, contents
  end


  def read_files(file_paths) do
    Enum.map file_paths, fn(file_path)->
      %{:path => file_path, :contents => File.read(file_path)}
    end
  end

end

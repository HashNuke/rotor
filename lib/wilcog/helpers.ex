defmodule Rotor.Helpers do


  def concat(files) do
    Enum.map_join(files, "\n", fn(file)->
      file.contents
    end)
  end


  def output_to(contents, output_path) do
    :ok = File.write output_path, contents
  end


  def read_files(file_paths) do
    Enum.map file_paths, fn(file_path)->
      {:ok, contents} = File.read(file_path)
      %{:path => file_path, :contents => contents}
    end
  end

end

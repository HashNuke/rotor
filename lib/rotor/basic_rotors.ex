defmodule Rotor.BasicRotors do


  def concat(files) do
    Enum.map_join(files, "\n", fn(file)->
      file.contents
    end)
  end


  def copy_files(files, destination_dir) do
    Enum.each files, fn(file)->
      File.copy(file.path, "#{destination_dir}/#{Path.basename(file.path)}")
    end
  end


  def output_to(contents, output_path) do
    :ok = File.write output_path, contents
  end


  def read_files(files) do
    Enum.map files, fn(file)->
      {:ok, contents} = File.read(file.path)
      %{file | :contents => contents}
    end
  end

end

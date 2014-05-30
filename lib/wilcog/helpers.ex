defmodule Wilcog.Helpers do

  def output(files, output_path) do
    concatenated_output = Enum.reduce files, fn(file, joined_content)->
      "#{joined_content}\n#{file.contents}"
    end
    File.write output_path, concatenated_output
  end


  def read_files(file_paths) do
    Enum.map file_path, fn(file_path)->
      %{:path => file_path, :contents => File.read(file_path)}
    end
  end

end

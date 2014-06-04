defmodule Rotor.Utils do


  def update_file_index_timestamps(current_index) do
    reducer = fn({path, file}, {changed_files, index})->
      case File.stat(path) do
        {:ok, stat} ->
          {changed_files, file} = if file.last_modified_at != stat.mtime do
            {
              changed_files ++ [file],
              %{file | :last_modified_at => stat.mtime}
            }
          else
            {changed_files, file}
          end
          updated_index = HashDict.put_new(index, path, file)
          {changed_files, updated_index}
        _ ->
          # Handle delted files
          {changed_files, index}
      end
    end

    Enum.reduce(current_index, {[], HashDict.new()}, reducer)
  end


  def build_file_index([], file_index) do
    file_index
  end


  def build_file_index([path | paths], file_index \\ HashDict.new) do
    updated_file_index = Enum.reduce Path.wildcard(path), file_index, fn(file_path, index)->
      {:ok, file_info} = File.stat(file_path)
      if file_info.type == :directory || HashDict.has_key?(index, file_path) do
        index
      else
        file_props = %{:path => file_path, :contents => nil, :last_modified_at => file_info.mtime}
        HashDict.put_new index, file_path, file_props
      end
    end

    build_file_index(paths, updated_file_index)
  end

end

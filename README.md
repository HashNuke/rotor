# Wilcog

> *[Wreckers][1] don't call for backup, they call for cleanup ~!*

[1]: http://en.wikipedia.org/wiki/Wreckers_(Transformers)

### Usage


```
Wilcog.add_group :javascripts, fn(files)->
  read_files(files)
  |> coffee
  |> output("app.js")
end

def add_group(group_name, paths)
  Wilcog.Server.call [:add_group, group_name, format_paths(paths)]
end


def format_paths(paths) do
  cond do
    :io_lib.char_list(paths) ->
    is_binary(paths) -> [paths]
    true -> paths
  end
  |> Enum.map(fn(path)-> "#{path}" end)
end
```

* add group: dir scan and add files to index
* refresh: loop thru all files in index and check timestamps, if timestamp changed trigger group
* add group:



maintain diff index for each group
check index for each group

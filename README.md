# Wilcog

> *[Wreckers][1] don't call for backup, they call for cleanup ~!*

[1]: http://en.wikipedia.org/wiki/Wreckers_(Transformers)

### Usage

```
Wilcog.for :styles, paths do
  run :coffee do |file|
    file.extension == ".coffee"
  end
  |> run(:concat, "app.js")
end


def for(group_name, paths)
  Wilcog.call :add_group, group_name, format_paths(paths)
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

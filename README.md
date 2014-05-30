# Rotor

#### Work in Progress

> *[Wreckers][1] don't call for backup, they call for cleanup ~!*

[1]: http://en.wikipedia.org/wiki/Wreckers_(Transformers)

### Usage


```
Rotor.add_group :javascripts, paths, fn(files)->
  read_files(files)
  |> coffee
  |> concat
  |> write_to("app.js")
end


Rotor.add_group :stylesheets, paths, fn(files)->
  read_files(files)
  |> concat
  |> write_to("app.css")
end
```

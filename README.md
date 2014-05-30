# Rotor

Build system for Elixir projects

#### Work in Progress

> *[Wreckers][1] don't call for backup, they call for cleanup ~!*

[1]: http://en.wikipedia.org/wiki/Wreckers_(Transformers)

### Usage


```
paths = ["assets/javascripts/libs/*.js"", "assets/javascripts/*.js"]
Rotor.add_group :javascripts, paths, fn(files)->
  read_files(files)
  |> concat
  |> write_to("app.js")
end


paths = ["assets/stylesheets/libs/*.css", "assets/stylesheets/*.css"]
Rotor.add_group :stylesheets, paths, fn(files)->
  read_files(files)
  |> concat
  |> write_to("app.css")
end


paths = ["assets/images/*", "assets/fonts/*"]
Rotor.add_group :images_and_fonts, paths, fn(files)->
  copy_files(files, "priv/static")
end
```

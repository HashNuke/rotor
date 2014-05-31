# Rotor

Build system for Elixir projects.

Use it to compile things, run commands or do anything that requires being run when files change.

> *[Wreckers][1] don't call for backup, they call for cleanup ~!*

[1]: http://en.wikipedia.org/wiki/Wreckers_(Transformers)

### Usage

A set of paths you want to watch is called a *watch group*. Each watch group has the following

* name
* a list of paths to watch
* a function, which we'll call the *pipeline*, that is run everytime any of the files in the paths changes. It should accept one argument (a list of maps, each having info about a file).


#### Adding watch groups

```
Rotor.add_group(name, files, pipeline_function)
```

The pipeline function is passed info about the list of files that match the paths specified. The pipeline function calls other functions `actions`, that run certain tasks.


```
paths = ["assets/javascripts/libs/*.js"", "assets/javascripts/*.js"]
Rotor.add_group :javascripts, paths, fn(files)->
  read_files(files)
  |> concat
  |> write_to("priv/static/assets/app.js")
end
```

#### Actions

Rotor ships with a few simple actions in the `Rotor.Actions` module.

* `read_files(files)` - reads contents of files, and returns files with a property called `contents`
* `copy_files(files, destination_dir)` - copies files to destination_dir
* `concat(files)` - concats contents of files and returns a string
* `write_to(contents, output_path)` - writes the contents to the file path specified in output path

You can also write your own. Check the *"Writing custom actions"* section below.


### Other stuff

To remove a watch group

```
Rotor.remove_group(group_name)
```

To list groups

```
Rotor.groups
```

To run a watch group's pipeline forcefully

```
Rotor.run(group_name)
```

### Examples

```
paths = ["assets/stylesheets/libs/*.css", "assets/stylesheets/*.css"]
Rotor.add_group :stylesheets, paths, fn(files)->
  read_files(files)
  |> concat
  |> write_to("app.css")
end


paths = ["assets/images/*", "assets/fonts/*"]
Rotor.add_group :images_and_fonts, paths, fn(files)->
  copy_files(files, "priv/static/assets")
end
```

### Writing custom actions

Actions are just functions that accept data and do something.

Checkout [coffee-rotor](https://github.com/HashNuke/coffee-rotor), which provides an action to compile CoffeeScript files.

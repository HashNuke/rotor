# Rotor

Rotor is a build system for Elixir projects. Use it to compile things, run commands or do anything that needs to be run when files change.

> *[Wreckers][1] don't call for backup, they call for cleanup ~!*

[1]: http://en.wikipedia.org/wiki/Wreckers_(Transformers)


### Example: Compile CoffeeScript files whenever they change

```elixir
paths = ["assets/libs/*.coffee", "assets/*.coffee"]
Rotor.add_group :coffee_assets, paths, fn(files)->
  read_files(files)
  |> coffee
  |> concat
  |> write_to("priv/static/assets/app.js")
end
```

*The above example uses the [coffee_rotor](https://github.com/HashNuke/coffee_rotor).*


> **NOTE:** Rotor is *not* a replacement for mix. It is intended to be used as your sidekick during development.


### Features

* Works with any web framework or even plain mix projects
* Easy to use
* Extendable with simple functions
* Can be configured to run commands or code or go to the moon.
* Inspired by [gulp](https://github.com/gulpjs/gulp)


### Usage

A set of paths you want to watch is called a *watch group*. Each watch group has the following

* name
* a list of paths to watch
* a function, which we'll call the *rotor function*, that is run everytime any of the files in the paths changes. It should accept one argument (a list of maps, each having info about a file).


#### Adding watch groups

```elixir
Rotor.add_group(name, files, rotor_function)
```

The rotor function is passed info about the list of files that match the paths specified. The rotor function calls other little functions called `rotors`, that run certain tasks.


```elixir
paths = ["assets/javascripts/libs/*.js", "assets/javascripts/*.js"]
Rotor.add_group :javascripts, paths, fn(files)->
  read_files(files)
  |> concat
  |> write_to("priv/static/assets/app.js")
end
```

#### Rotors

Rotor ships with a few simple rotors in the `Rotor.Basic` module.

* `read_files(files)` - reads contents of files, and returns files with a property called `contents`
* `copy_files(files, destination_dir)` - copies files to destination_dir
* `concat(files)` - concats contents of files and returns a string
* `write_to(contents, output_path)` - writes the contents to the file path specified in output path

You can also write your own. Check the *"Writing custom rotors"* section below.


### Other stuff

* To remove a watch group

    ```elixir
    Rotor.remove_group(group_name)
    ```

* To list groups

    ```elixir
    Rotor.groups
    ```

* To run a watch group's rotor functions forcefully

    ```elixir
    Rotor.run(group_name)
    ```

### Examples

```elixir
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

### Writing custom rotors

Rotors are just functions that accept data and do something.

Checkout [coffee_rotor](https://github.com/HashNuke/coffee_rotor), which provides a rotor to compile CoffeeScript files.


### License

Copyright © 2014, Akash Manohar J, under the [MIT License](http://opensource.org/licenses/MIT)

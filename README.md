# Rotor

Rotor is a build system for Elixir projects. Use it to compile things, run commands or do anything that needs to be run when files change.

> *[Wreckers][1] don't call for backup, they call for cleanup ~!*

[1]: http://en.wikipedia.org/wiki/Wreckers_(Transformers)


**Define your rotor watch groups in `config/rotors.exs` in your project and they'll be loaded when your app starts**


### Example 1: Reload Elixir modules whenever they change

```elixir
# config/rotors.exs

use Rotor.Config

paths = ["lib/**/*"]

Rotor.define :ex_modules, paths, fn(changed, _all)->
  reload_modules(changed)
end
```

Make changes to any file in the lib dir of your project and watch it reload in your console

### Example 2: Compile CoffeeScript files whenever they change

```elixir
# config/rotors.exs

use Rotor.Config

paths = ["assets/libs/*.coffee", "assets/*.coffee"]
Rotor.define :coffee_assets, paths, fn(changed_files, all_files)->
  read_files(all_files)
  |> coffee
  |> concat
  |> write_to("priv/static/assets/app.js")
end

```

`touch` a file that's in the path provided and watch the rotor function being run.

*The above example uses the [coffee_rotor](https://github.com/HashNuke/coffee_rotor).*


> **NOTE:** Rotor is *not* a replacement for mix. It is intended to be used as your sidekick during development.


### Features

* Works with any web framework or even plain mix projects
* Easy to use
* Extendable with simple functions
* Can be configured to run commands or code or go to the moon.
* Inspired by [gulp](https://github.com/gulpjs/gulp)
* Have as many as rotors as you want

### Usage

#### Where to define watch groups?

`config/rotors.exs`.

#### What is needed?

Each watch group, has a set of paths you want to watch and has the following

* name
* a list of paths to watch
* *rotor function* - a function that is run everytime any of the files in the paths changes. It should accept 2 arguments
  * *changed_files* - a list of maps, each containing info about a changed file
  * *all_files* - a list of maps, each containing info about all files that matched the path


#### How to define watch groups?

```elixir
# With default options
Rotor.define(name, files, rotor_function)

# With options
Rotor.define(name, files, rotor_function, options)
```

The rotor function is passed info about the list of files that match the paths specified. The rotor function calls other little functions called `rotors`, that run certain tasks.


```elixir
paths = ["assets/javascripts/libs/*.js", "assets/javascripts/*.js"]
Rotor.define :javascripts, paths, fn(changed_files, all_files)->
  read_files(all_files)
  |> concat
  |> write_to("priv/static/assets/app.js")
end
```

The fourth argument is options. It accepts a map. The following are valid options:

* `manual` - defaults to false. If set to true, paths will only be polled when `Rotor.run/1` or `Rotor.run_async/1` is called.
* `interval` - defaults to 2500 milliseconds (2.5 seconds). This is the interval at which files are checked for changes.


#### Manually running watch group rotor function

If you want files to be polled only when you say so (and not at intervals). Then pass the `manual` option as `true` when adding a group. Then use one of the following functions to trigger a poll.

* `Rotor.run(group_name)` - will poll paths and run the Rotor function synchronously
* `Rotor.run_async(group_name)` - will poll paths and run the Rotor function asynchronously



#### Rotors

Rotor ships with a few simple rotors in the `Rotor.Basic` module.

* `read_files(files)` - reads contents of files, and returns files with a property called `contents`
* `copy_files(files, destination_dir)` - copies files to destination_dir
* `concat(files)` - concats contents of files and returns a string
* `write_to(contents, output_path)` - writes the contents to the file path specified in output path
* `reload_modules(files)` - reloads the modules in the list of files passed

You can also write your own. Check the *"Writing custom rotors"* section below.


### Other stuff

* To remove a watch group

    ```elixir
    Rotor.stop_watching(name)
    ```

* To list all watch groups

    ```elixir
    Rotor.all
    ```

* To run a watch group's rotor function forcefully

    ```elixir
    Rotor.run(name)
    ```

### Examples

```elixir
paths = ["assets/stylesheets/libs/*.css", "assets/stylesheets/*.css"]
Rotor.define :stylesheets, paths, fn(changed_files, all_files)->
  read_files(all_files)
  |> concat
  |> write_to("app.css")
end


paths = ["assets/images/*", "assets/fonts/*"]
Rotor.define :images_and_fonts, paths, fn(changed_files, all_files)->
  copy_files(files, "priv/static/assets")
end
```

### Writing custom rotors

Rotors are just functions that accept data and do something.

Checkout [coffee_rotor](https://github.com/HashNuke/coffee_rotor), which provides a rotor to compile CoffeeScript files.


### License

Copyright © 2014, Akash Manohar J, under the [MIT License](http://opensource.org/licenses/MIT)

# wilcog

__Work in Progress__

> Erlang asset pipeline

#### Features

* Inbuilt support for SCSS, Coffeescript and Handlebars templates
* CSS and JavaScript compression
* Sprockets directives
* Asset digests for cache busting
* **EXTENDABLE** ~!!!

And most importantly, does not require Ruby or Node.js installed :)

## Conventions

### File extensions

If you have a file with name `example.abc.rst.xyz`, then the file is passed through compilers in the reverse order of extensions (`xyz`, then `rst` and so on). The compilation process is stopped extension with no registered compilers is encountered.

So if your file is named, `example.js.coffee`, the compiler for `coffee` files will be used first. And since there's no compiler registered for `js`, the output file will be called `example.js`.

There is a way to change the extension, but that depends on the compilers registered.

### Directives

Directives must begin at the start of the file. wilcog will assume the first empty line as the end of directives.

#### CSS and SCSS directives

TODO

#### Javascript and CoffeeScript directives

TODO

## Usage

#### Start application

```
application:start(wilcog)
```

#### Compile assets

wilcog adds application.js, application.css and every non-JS/CSS asset to the output dir. Anything else must be added explicitly.

If the output dir doesn't exist, wilcog will attempt to create it. The output dir can be safely added to your `.gitignore`.


```
%% wilcog:compile(AssetPath, OutputDir)
wilcog:compile(<<"assets">>, <<"priv/static/assets">>)


%% Incase you want something else to be compiled seperately,
%% list extra assets you need compiled.
%% The list should contain expected names of assets.
%% So if your coffeescript file is called `example.js.coffee`
%% and you need it pre-compiled, then use `example.js` in the list.

%% wilcog:compile(AssetPath, OutputDir, PreCompileList)
wilcog:compile(<<"assets">>, <<"priv/static/assets">>, [<<"example.js">>, <<"somethingelse.css">>])


%% You can also pass options as the last argument
Options = [{<<"digest">>, false}, {<<"compress">>, false}}]

wilcog:compile( <<"assets">>, <<"priv/static/assets">>, Options)

wilcog:compile(<<"assets">>, <<"priv/static/assets">>, [<<"example.js">>], Options)
```

`compress` and `digest` are the only two options available now. Valid values for both are booleans.


## Extending with your own compilers

Let's say you have templates with the ".sky" extension that is to be compiled.

#### Write your compiler

You'll need your own module which defines `compile/2`. The following are the arguments that'll be passed:

* source - source string to be compiled.
* meta data - a proplist with details like file_name, modified date, etc. Mostly stuff you won't require, but just incase.
* options - options set for wilcog are passed down to the compilers too.

Here's an example module for compiling files with `sky` extension:

```
-module(awesome_sky_compiler).
-exports([compile/3]).

compile(SourceString, MetaData, Options)->
  %%% assuming you do some magic here to compile the Sky code,
  %%% return the following
  {ok, OutputString, Options}
```

The `OutputString` is pretty obvious. `Options` is a list and can include the following:

* `{<<"force_extension">>, SomeExtensionName}` - Specify an extension (as binary) that is to be used for the resulting file. The first compiler that sets this, wins. Never use this unless you are desperate about being an overlord when it comes to file extensions. Like `{<<"force_extension">>, <<".js">>}`

Incase of error return `{error, Reason}` and it'll be output.

#### Register your compiler

To teach wilcog about compiling `.sky` files, register it. Make sure wilcog is already started before you do this.

```
ok = wilcog:add_compiler(<<"sky">>, awesome_sky_compiler).
```

## Credits

And here goes a virtual hug to whoever thought of directives in Sprockets.

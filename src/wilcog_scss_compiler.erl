-module(wilcog_scss_compiler).
-export([compile/3]).

compile(Source, MetaData, Options)->
  StylishOptions = get_stylish_options(Options),
  case stylish:compile(Source, StylishOptions) of
    {ok, OutputString} ->
      {ok, OutputString};
    {error, Error} ->
      {ok, Error}
  end.


get_stylish_options(Options)->
  case proplists:get_value(<<"compressed">>, Options, false) of
    true ->
      [{"style", "compressed"}];
    _ ->
      []
  end.

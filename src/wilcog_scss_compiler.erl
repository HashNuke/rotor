-module(wilcog_scss_compiler).
-export([compile/3]).

compile(Source, MetaData, Options)->
  StylishOptions = get_stylish_options(Options),
  Extensions = proplists:get_value(<<"extensions">>, MetaData),
  case length(Extensions) of
    1 -> {ok, ""}; % because we dont compile if it's an scss partial
    _ -> {ok, stylish:compile(Source, StylishOptions)}
  end.


get_stylish_options(Options)->
  Compress = proplists:get_value(<<"compress">>, Options, false),
  [{<<"compress">>, Compress}].

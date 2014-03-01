-module(wilcog_scss_compiler).
-export([compile/3]).

compile(Source, _MetaData, Options)->
  StylishOptions = get_stylish_options(Options),
  stylish:compile(Source, StylishOptions).


get_stylish_options(Options)->
  Compress = proplists:get_value(<<"compress">>, Options, false),
  [{<<"compress">>, Compress}].

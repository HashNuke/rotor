-module(wilcog_default_compiler).
-export([compile/3]).

compile(_Source, MetaData, Options)->
  Path = proplists:get_value(<<"path">>, MetaData),
  OutputPath = proplists:get_value(<<"output_path">>, Options),
  File = filename:basename(Path),
  Parts = string:tokens(File, "."),
  MainName = hd(Parts),

  ExtensionName = case length(Parts) of
    1 ->
      hd(Parts);
    _ ->
      hd(tl(Parts))
  end,
  OutputName = string:join([MainName, ExtensionName], "."),
  OutputFile = filename:absname_join(binary_to_list(OutputPath), OutputName),
  {ok, _} = file:copy(Path, OutputFile),
  {ok, ""}.

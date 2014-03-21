-module(wilcog_directive_parser).
-export([parse/1]).


parse(File) ->
  Contents = read_file(File),
  {ok, DirectiveBlocks, _} = wilcog_directive_block_scanner:string(Contents),
  parse_directives(DirectiveBlocks).


parse_directives([]) ->
  [];
parse_directives([ DirectiveBlock | _ ]) ->
  {directive_block, _, TokenString} = DirectiveBlock,
  {ok, Tokens, _} = wilcog_directive_scanner:string(TokenString),

  FilterMapFunction = fun({Label, _, TokenString})->
    case Label == directive of
      true -> {true, TokenString};
      false -> false
    end
  end,

  Directives = lists:filtermap(FilterMapFunction, Tokens),
  parse_dependencies(Directives).


parse_dependencies(Directives) ->
  MapperFunction = fun(Directive) ->
    Command = string:tokens(Directive, " "),
    get_dependency_from_require(Command)
  end,
  lists:map(MapperFunction, Directives).


get_dependency_from_require(["require_self"]) ->
  self;
get_dependency_from_require(["require_tree", RelativePath]) ->
  {tree, RelativePath};
get_dependency_from_require(["require", Path]) ->
  {file, Path}.


read_file(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  get_all_lines(Device, []).


get_all_lines(Device, Accum) ->
  case io:get_line(Device, "") of
    eof  -> file:close(Device), Accum;
    Line -> get_all_lines(Device, Accum ++ [Line])
  end.
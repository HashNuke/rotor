-module(wilcog_file_tree).
-export([build/1, build/3]).

build(Path) ->
  Graph = digraph:new([acyclic]),
  {ok, DirList} = file:list_dir(Path),
  digraph:add_vertex(Graph, Path, [{type, dir}]),
  build(Path, DirList, Graph).


build(_Parent, [], Graph) ->
  Graph;
build(Parent, [Item|Items], Graph) ->
  ItemPath = filename:absname_join(Parent, Item),
  Graph = case filelib:is_dir(ItemPath) do
    true ->
      {ok, DirList} = file:list_dir(ItemPath),
      Vertex = digraph:add_vertex( Graph, ItemPath, [{type, dir}, {dependencies, []}] ),
      digraph:add_edge(Graph, Parent, Vertex),
      build(ItemPath, DirList, Graph);
    false ->
      Props = file_properties(ItemPath),
      Vertex = digraph:add_vertex(Graph, ItemPath, Props),
      digraph:add_edge(Graph, Parent, Vertex),
      Graph
  end,
  build(Parent, Items, Graph).


file_properties(Path) ->
  FileBaseName = filename:basename(Path),
  FileInfo = wilcog_file_utils:extract_info(FileBaseName),
  Props = lists:merge(FileInfo, [{type, file}]),
  OutputFileName = proplists:get_value(output, Props),

  case is_js_or_css(OutputFileName) of
    true ->
      props ++ [{ dependencies, wilcog_directive_parser:parse(Path) }];
    false ->
      props
  end.


is_js_or_css(OutputName) ->
  Parts = string:tokens(OutputName, "."),
  lists:member(lists:last(Parts), ["js", "css", "coffee", "scss"]).

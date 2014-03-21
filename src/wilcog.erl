-module(wilcog).
-export([compile/2, compile/3]).


compile(AssetPath, OutputPath)->
  compile(AssetPath, OutputPath, []);
compile(AssetPath, OutputPath, Options)->
  DefaultPrecompileList = ["application.js", "application.css"],
  ExtraPrecompileList = proplists:get_value(precompile, Options, []),
  PrecompileList = DefaultPrecompileList ++ ExtraPrecompileList,
  Graph = wilcog_file_tree:build(AssetPath),

  PrecompileVertices = get_vertices_of_precompile_list(Graph, PrecompileList),
  create_dir_if_not_exists(OutputPath),

  compile_assets(PrecompileVertices, Graph, OutputPath, Options).


get_vertices_of_precompile_list(Graph, PrecompileList)->
  Elements = digraph_utils:topsort(Graph),

  FoldFunction = fun(Name, {PrecompileVertices, PrecompileList})->
    {Vertex, Data} = digraph:vertex(Graph, Name),
    OutputFileName = proplists:get_value(output, Data),
    if
      lists:member(OutputFileName, PrecompileList) ->
        {PrecompileVertices ++ [Vertex], PrecompileList};
      true ->
        {PrecompileVertices, PrecompileList}
    end,
  end,
  {PrecompileVertices, _} = lists:foldr(FoldFunction, {[], PrecompileList}, Elements),
  PrecompileVertices.


create_dir_if_not_exists(Path) ->
  if
    !filelib:is_dir(Path) ->
      ok = file:make_dir(Path);
    true ->
      ok
  end.


write_file(FilePath, Contents) ->
  {ok, IODevice} = file:open(FilePath, [write]),
  file:write(IODevice, Contents),
  file:close(IODevice).


compile_assets([], _Graph, _OutputPath, _Options) ->
  ok;
compile_assets([Vertex|OtherVertices], Graph, OutputPath, Options) ->
  {_, Data} = digraph:vertex(Graph, Vertex),
  Dependencies = proplists:get_value(dependencies, Data),
  OutputFileName = proplists:get_value(output, Data),
  CompiledDependencies = compile_dependencies(Vertex, Dependencies, Graph, Options),
  Contents = string:join(CompiledDependencies, " "),

  %% current_file_contents = compile_without_dependencies(vertex, vertex_data, graph, options)
  write_file(OutputPath ++ "/" ++ OutputFileName, Contents),
  compile_assets(OtherVertices, Graph, OutputPath, Options).


compile_dependencies(ParentFile, Dependencies, Graph, Options) ->
  Mapper = fun(DependencyInfo)->
    case DependencyInfo of
      self ->
        "";
      {file, Dependency} ->
        DependencyVertex = guess_vertex(Dependency, "file", ParentFile, Graph),
        {_, DependencyVertexData} = digraph:vertex(Graph, DependencyVertex),
        compile_file(Dependency, DependencyVertex_data, Graph, Options);
      {tree, Dependency} ->
        dependency_vertex = guess_vertex(Dependency, "dir", ParentFile, Graph),
        {_, dependency_vertex_data} = digraph:vertex(Graph, DependencyVertex),
        compile_dir(Dependency, DependencyVertex_data, Graph, Options)
    end
  end,
  lists:map(Mapper, Dependencies).


compile_dir(Vertex, VertexData, Graph, Options)->
  "definitely".

compile_without_dependencies(Vertex, VertexData, Graph, Options)->
  "definitely".

compile_file(Vertex, VertexData, Graph, Options)->
  "definitely".


guess_vertex(AssetName, Type, ReferenceVertex, Graph)->
  Pattern = wilcog_file_utils:possible_path_relative_to_file(ReferenceVertex, AssetName),
  Vertices = digraph_utils:topsort(Graph),
  Result = find_matching_vertex(Vertices, Type, Pattern, Graph),
  if
    Result == undefined ->
      erlang:error("No match found for " ++ AssetName ++ ". Referenced in " ++ ReferenceVertex);
    true -> Result
  end.


find_matching_vertex([], _LookupVertexType, _Pattern, _Graph)->
  undefined;
find_matching_vertex([Vertex | Others], LookupVertexType, Pattern, Graph)->
  Tokens = re:split(Vertex, Pattern, [{return, list}]),
  {_, VertexData} = digraph:vertex(Graph, Vertex),

  if
    proplists:get_value(type, VertexData) != LookupVertexType ->
      find_matching_vertex(Others, Type, Pattern, Graph);
    hd(Tokens) == [] ->
      vertex;
    true ->
      find_matching_vertex(Others, Type, Pattern, Graph)
  end.

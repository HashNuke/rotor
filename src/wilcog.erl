-module(wilcog).
-export([build/1, build/2, rebuild/3, rebuild/4]).


build(Path) ->
  Options = [{<<"digest">>, true}, {<<"compress">>, false}],
  build(Path, Options).


build(Path, Options) ->
  OldTree = gb_trees:empty(),
  rebuild(Path, OldTree, Options).


rebuild(Path, OldTree, Options)->
  NewTree = gb_trees:empty(),
  PathProperties = [{"path", Path}],
  RootTree = gb_trees:enter(Path, PathProperties, NewTree),
  rebuild(Path, RootTree, OldTree).


rebuild(Path, Tree, OldTree, Options)->
  {ok, Items} = file:list_dir_all(Path),

  ItemFolder = fun(Item, Acc)->
    {ParentPath, AccumulatedTree, OldTree} = Acc,
    ItemPath = filename:absname_join(ParentPath, Item),
    DefaultProps = [{"path", ItemPath}],

    case filelib:is_dir(ItemPath) of
      true -> % It's a dir. Recurse through it and update tree.
        NewTree = gb_trees:enter(ItemPath, DefaultProps, AccumulatedTree),
        UpdatedTree = rebuild(ItemPath, NewTree, OldTree),
        {ParentPath, UpdatedTree, OldTree};
      false -> % It's a file. Return updated tree.
        NewStamp = filelib:last_modified(ItemPath),
        case NewStamp of
          0 ->
            % File deleted. So just return whatever required
            {ParentPath, AccumulatedTree, OldTree};
          _ ->
            % File exists, Add output to tree
            Output = get_output_for(ItemPath, NewStamp, OldTree),
            FileProps = [{"modified_at", NewStamp}, {"compiled", Output}] ++ DefaultProps,
            UpdatedTree = gb_trees:enter(ItemPath, FileProps, AccumulatedTree),
            {ParentPath, UpdatedTree, OldTree}
        end
    end
  end,

  % The accumulator is weird, because we want function to be free of parent scope.
  % Or atleast try to.
  {_, FinalTree, _} = lists:foldl(ItemFolder, {Path, Tree, OldTree}, Items),
  FinalTree.


get_output_for(ItemPath, NewStamp, OldTree) ->
  case gb_trees:lookup(ItemPath, OldTree) of
    {value, OldProps} ->
      OldStamp = proplists:get_value("modified_at", OldProps),
      case NewStamp of
        undefined ->
          compile_file(ItemPath);
        OldStamp ->
          proplists:get_value("compiled", OldProps);
        _ ->
          compile_file(ItemPath)
      end;
    none ->
      compile_file(ItemPath)
  end.


compile_string(String, FileInfo, Options) ->
  Compilers = [{"scss", wilcog_scss_compiler}],
  File = proplists:get_value(<<"path">>, FileInfo),
  Extensions = tl(string:tokens(File, ".")),

  case length(Extensions) of
    0 -> "";
    _ ->
      RunExtensions = fun(Extension, Acc)->
        {Source, MetaData, Options} = Acc,
        ExtensionCompiler = proplists:get_value(Extension, Compilers, wilcog_default_compiler),
        {Output, UpdatedOptions} = compile(ExtensionCompiler, Source, MetaData, Options),
        {Output, MetaData, UpdatedOptions}
      end,

      FileMeta = [{<<"extensions">>, Extensions} | FileInfo],
      {CompiledOutput, _, _} = list:foldl(RunExtensions, {String, FileMeta, Options}, Extensions),
      CompiledOutput
  end.


compile_file(Path)->
  File = filename:basename(Path),

  case file:read_file(File) of
    {ok, FileContents} ->
      compile_string(FileContents, [{<<"path">>, File}], []);

    {error, Reason}->
      erlang:display(File),
      erlang:display(Reason),
      "" %because the other files can be compiled
  end.


compile(Compiler, Source, MetaData, Options)->
  case Compiler:compile(Source, MetaData, Options) of
    {ok, Output, _ReturnedOptions} ->
      % TODO actually merge the returned options with defaults
      {Output, Options};
    {ok, Output} ->
      {Output, Options}
  end.

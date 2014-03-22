-module(wilcog_file_utils).
-export([possible_path_relative_to_file/2]).


possible_path_relative_to_file(File, RelativePath) ->
  ParentPath = filename:dirname(File),

  % We don't use filename:absname because we need the correct absolute path
  % to fetch from the file tree graph
  Parts = string:tokens(RelativePath, "/"),

  % the parent dir is fixed (something like cd-ed) according to the "..",
  % while the include path's ".." is popped

  FoldFunction = fun(Part, {ParentPathParts, IncludePathParts}) ->
    case Part of
      ".." ->
        ReversedParentPathParts = lists:reverse(ParentPathParts),
        NewParentPathParts = lists:reverse( tl(ReversedParentPathParts) ),
        {NewParentPathParts, tl(IncludePathParts)};
      "." ->
        {ParentPathParts, tl(IncludePathParts)};
      _ ->
        {ParentPathParts, IncludePathParts}
    end
  end,

  InitialAccumulatorValue = {string:tokens(ParentPath, "/"), Parts},
  {ParentParts, IncludeParts} = lists:foldl(FoldFunction, InitialAccumulatorValue, Parts),
  "/" ++ string:join(ParentParts ++ IncludeParts, "/").


compiler_for(Extension) ->
  proplists:get_value(Extension, compilers()).


compilers() ->
  [{"scss", wilcog_scss_compiler},
   {"js", wilcog_javascript_compiler},
   {"css", wilcog_css_compiler},
   {"coffee", wilcog_coffeescript_compiler}].


compiled_name_for(_SourceFilename, Basename, []) ->
  Basename;
compiled_name_for(SourceFilename, Basename, KnownExtensions) ->
  Parts = [Basename] ++ [compute_extension(SourceFilename, lists:last(KnownExtensions))],
  string:join(Parts, ".").


compute_basename(Part1, []) ->
  Part1;
compute_basename(Part1, OtherParts) ->
  string:join([Part1 | OtherParts], ".").


compute_extension(SourceFilename, Extension) ->
  case is_valid_extension(Extension) of
    true ->
      Compiler = compiler_for(Extension),
      Compiler:expected_extension(SourceFilename);
    _ -> Extension
  end.

is_valid_extension(Extension) ->
  compiler_for(Extension) and is_extension_defined(compiler_for(Extension)).


is_extension_defined(Module)->
  ModuleExports = proplists:get_value(exports, Module),
  ExpectedExtension = proplists:get_value(expected_extension, ModuleExports, 0),
  ExpectedExtension == 1.


extract_info(SourceFilename) ->
  Parts = string:tokens(SourceFilename, "."),
  FirstPart = hd(Parts),
  {KnownExtensions, UnknownExtensions} = group_extensions( tl(Parts) ),

  ReversedUnknownExtensions = if
    UnknownExtensions /= [] ->
      UnknownExtensions = lists:reverse(UnknownExtensions);
    true -> []
  end,

  Basename = compute_basename(FirstPart, ReversedUnknownExtensions),
  CompiledName = compiled_name_for(SourceFilename, Basename, KnownExtensions),

  [
    {source, SourceFilename},
    {output, CompiledName},
    {compilers, KnownExtensions}
  ].


group_extensions([]) ->
  {[], []};
group_extensions(Extensions) ->
  ReversedExtensions = lists:reverse(Extensions),
  PartitionFunction = fun(Extension)->
    lists:member(Extension, proplists:get_keys(compilers()) )
  end,
  lists:partition(PartitionFunction, ReversedExtensions).

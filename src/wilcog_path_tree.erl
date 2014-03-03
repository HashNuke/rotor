-module(wilcog_path_tree).
-export([build/1, build/2, rebuild/2]).

build(Path)->
  NewTree = gb_trees:empty(),
  PathProperties = [{"path", Path}],
  RootTree = gb_trees:enter(Path, PathProperties, NewTree),
  build(Path, RootTree).


build(Path, Tree)->
  {ok, Items} = file:list_dir_all(Path),

  ItemFolder = fun(Item, Acc)->
    {ParentPath, AccumulatedTree} = Acc,
    ItemPath = filename:absname_join(ParentPath, Item),
    LastModifiedAt = filelib:last_modified(ItemPath),
    ItemProperties = [{"path", ItemPath}],

    case filelib:is_dir(ItemPath) of
      true ->
        UpdatedTree = gb_trees:enter(ItemPath, ItemProperties, AccumulatedTree),
        {ItemPath, build(ItemPath, UpdatedTree)};
      _ ->
        FileProps = [{"modified_at", LastModifiedAt} | ItemProperties],
        {ItemPath, gb_trees:enter(ItemPath, FileProps, AccumulatedTree)}
    end
  end,

  {_, FinalTree} = lists:foldl(ItemFolder, {Path, Tree}, Items),
  FinalTree.


rebuild(Path, OldTree)->
  NewTree = build(Path),
  NewKeys = gb_trees:keys(NewTree),


  ItemFold = fun(ItemPath, Acc)->
    {CurrentTree, OldTree} = Acc,
    NewStamp = filelib:last_modified(ItemPath),
    NewProps = [
      {"path", ItemPath},
      {"modified_at", NewStamp}
    ],

    Output = case gb_trees:lookup(ItemPath, OldTree) of
      {value, OldProps} ->
        OldStamp = proplists:get_value("modified_at", OldProps),
        case NewStamp of
          OldStamp ->
            proplists:get_value("compiled", OldProps);
          _ ->
            todo %TODO
        end;
      none ->
        todo %TODO
    end,
    UpdatedTree = gb_trees:enter(ItemPath, [{"compiled", Output} | NewProps], CurrentTree),
    {UpdatedTree, OldTree}
  end,


  {Finaltree, _ } = lists:foldl(ItemFold, {NewTree, OldTree}, NewKeys).
  FinalTree.

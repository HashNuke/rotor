-module(wilcog_path_tree).
-export([build/1, build/2]).

build(Path)->
  NewTree = gb_trees:empty(),
  PathProperties = [{"path", Path}],
  RootTree = gb_trees:enter(Path, PathProperties, NewTree),
  build(Path, RootTree).


build(Path, Tree)->
  {ok, Items} = file:list_dir_all(Path),

  ItemFolder = fun(Item, AccumulatedTree)->
    ParentProps = gb_trees:get(Path, AccumulatedTree),
    ParentPath = proplists:get_value("path", ParentProps),
    ItemPath = filename:absname_join(ParentPath, Item),
    LastModifiedAt = filelib:last_modified(ItemPath),
    ItemProperties = [{"path", ItemPath}],

    case filelib:is_dir(ItemPath) of
      true ->
        UpdatedTree = gb_trees:enter(ItemPath, ItemProperties, AccumulatedTree),
        build(ItemPath, UpdatedTree);
      _ ->
        FileProps = [{"modified_at", LastModifiedAt} | ItemProperties],
        gb_trees:enter(ItemPath, FileProps, AccumulatedTree)
    end
  end,
  lists:foldl(ItemFolder, Tree, Items).

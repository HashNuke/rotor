-module(wilcog_path_tree).
-export([build_tree/1, build_tree/2]).

build_tree(Path)->
  NewTree = gb_trees:empty(),
  PathProperties = [{"path", Path}],
  RootTree = gb_trees:enter(Path, PathProperties, NewTree),
  build_tree(Path, RootTree).

build_tree(Path, Tree)->
  {ok, Items} = file:list_dir_all(Path),

  ItemMapper = fun(Item)->
    ParentProps = gb_trees:get(Path, Tree),
    ParentPath = proplists:get_value("path", ParentProps),
    ItemPath = filename:absname_join(ParentPath, Item),
    LastModifiedAt = filelib:last_modified(ItemPath),
    ItemProperties = [
      {"path", ItemPath}
    ],

    case filelib:is_dir(ItemPath) of
      true ->
        erlang:display("Dir"),
        erlang:display(ItemPath),
        UpdatedTree = gb_trees:enter(ItemPath, ItemProperties, Tree),
        build_tree(ItemPath, UpdatedTree);
      _ ->
        erlang:display("File"),
        erlang:display(ItemPath),
        gb_trees:enter(ItemPath, [{"modified_at", LastModifiedAt} | ItemProperties], Tree)
    end
  end,
  lists:map(ItemMapper, Items).

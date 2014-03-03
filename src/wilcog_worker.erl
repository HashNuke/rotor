-module(wilcog_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([rebuild/1, watch/2]).
-export([init/1, handle_call/3]).


rebuild(Group) ->
    gen_server:call(wilcog_worker, {rebuild, Group}).

watch(Group, Options) ->
  gen_server:call(wilcog_worker, {watch, Group, Options}).


start_link() ->
  gen_server:start_link({local, wilcog_worker}, ?MODULE, [], []).


init(_Args) ->
  {ok, dict:new()}.


handle_call({watch, Group, AssetPath, Options}, _From, State) ->
  case dict:find(Group, State) of
    error ->
      InitialTree = wilcog:build(AssetPath, Options),
      NewState = dict:store(Group, {AssetPath, InitialTree, Options}, State),
      {reply, ok, NewState};
    {ok, _} ->
      {reply, group_already_exists, State}
  end;


handle_call({rebuild, Group}, _From, State) ->
  case dict:find(Group, State) of
    {ok, {AssetPath, Tree, Options}} ->
      NewTree = wilcog:rebuild(AssetPath, Tree, Options),
      NewState = dict:store(Group, {AssetPath, NewTree, Options}, State),
      {reply, ok, NewState};
    error ->
      {reply, no_group_found, State}
  end.

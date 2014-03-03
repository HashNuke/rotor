-module(wilcog_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([rebuild/1, watch/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



rebuild(Group) ->
  gen_server:call(wilcog_worker, {rebuild, Group}).

watch(Group, AssetPath, Options) ->
  gen_server:call(wilcog_worker, {watch, Group, AssetPath, Options}).


start_link() ->
  gen_server:start_link({local, wilcog_worker}, ?MODULE, [], []).


init(_Args) ->
  {ok, dict:new()}.


handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};


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


handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

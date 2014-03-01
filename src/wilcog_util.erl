-module(wilcog_util).
-exports([is_proplist/1]).

%% Code was borrowed from
%% https://code.google.com/p/zotonic/source/browse/src/support/z_utils.erl?r=d9da1b9de75c2567aca49a22548e05c89808d94a

is_proplist([]) -> true;
is_proplist([{K,_}|R]) when is_atom(K) -> is_proplist(R);
is_proplist(_) -> false.

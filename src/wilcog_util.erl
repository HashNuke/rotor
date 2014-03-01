-module(wilcog_util).
-exports([is_proplist/1]).

is_proplist([]) -> true;
is_proplist([{K,_}|R]) when is_atom(K) -> is_proplist(R);
is_proplist(_) -> false.

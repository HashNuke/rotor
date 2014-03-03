-module(wilcog_util).
-exports([is_proplist/1, binaries_to_list/1, clean_option_keys/1]).


binaries_to_list(List)->
  lists:map(fun(Value)->
    case is_binary(Value) of
      true  -> binary_to_list(Value);
      false -> Value
    end
  end, List).


clean_option_keys(Options)->
  lists:map(fun({Key, Value})->
    case is_binary(Key) of
      true  -> {binary_to_list(Key), Value};
      false -> {Key, Value}
    end
  end, Options).



%% Code was borrowed from
%% https://code.google.com/p/zotonic/source/browse/src/support/z_utils.erl?r=d9da1b9de75c2567aca49a22548e05c89808d94a

is_proplist([]) -> true;
is_proplist([{K,_}|R]) when is_atom(K) -> is_proplist(R);
is_proplist(_) -> false.

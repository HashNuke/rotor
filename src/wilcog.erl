-module(wilcog).
-export([request_compile/2]).


request_compile(Group, SubPath)->
  %% TODO imaginary
  gen_server.call(:requested, [SubPath])

-module(sherll_helper).

-export([process_register/2]).

process_register(Name, Pid) ->
   case whereis(Name) of
      undefined -> register(Name, Pid);
      _Else -> ok
   end.

%%%-------------------------------------------------------------------
%%% @author geomslayer
%%% Created : 28. Янв. 2018 15:44
%%%-------------------------------------------------------------------
-module(main).
-author("geomslayer").

-export([main/0]).

fact(N) ->
  if
    N =< 1 -> 1;
    true -> N * fact(N - 1)
  end.

main() ->
  Numbers = [1000, 1, 5, 10],
  Result = utils:parallel_map(fun fact/1, Numbers),
  io:fwrite("~w ~w\n", [Numbers, Result]).

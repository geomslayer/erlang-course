%%%-------------------------------------------------------------------
%%% @author geomslayer
%%% Created : 28. Янв. 2018 15:57
%%%-------------------------------------------------------------------
-module(parallel).
-author("geomslayer").

-export([eval_func_and_send_back/3, map/2]).

eval_func_and_send_back(ParentPid, TargetFunction, {Index, Element}) ->
  Result = TargetFunction(Element),
  ParentPid ! {Index, Result}.

spawn_and_eval(TargetFunction, {Index, Element}) ->
  pool:pspawn(?MODULE, eval_func_and_send_back, [
    self(),
    TargetFunction,
    {Index, Element}
  ]).

launch_next(Values, Index, TargetFunction) ->
  if
    length(Values) > 0 ->
      [Current | Rest] = Values,
      spawn_and_eval(TargetFunction, {Index, Current}),
      launch_next(Rest, Index + 1, TargetFunction);
    true -> ok
  end.

launch_map(Values, TargetFunction) -> launch_next(Values, 0, TargetFunction).

map(TargetFunction, Values) ->
  pool:start(node()),
  launch_map(Values, TargetFunction),
  Results = lists:map(
    fun(_) ->
      receive
        {Index, Result} -> {Index, Result}
      end
    end,
    Values),
  pool:stop(),
  lists:map(fun({_, Value}) -> Value end, lists:sort(Results)).

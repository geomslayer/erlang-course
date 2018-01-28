%%%-------------------------------------------------------------------
%%% @author geomslayer
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Янв. 2018 15:57
%%%-------------------------------------------------------------------
-module(utils).
-author("geomslayer").

%% API
-export([eval_func_and_send_back/3, parallel_map/2]).


eval_func_and_send_back(ParentPid, TargetFunction, Element) ->
  Result = TargetFunction(Element),
  ParentPid ! {self(), Result}.

spawn_and_eval(TargetFunction, Element) ->
  spawn(?MODULE, eval_func_and_send_back, [
    self(),
    TargetFunction,
    Element
  ]).

construct_indices(ChildPids) ->
  iter_pids(maps:new(), ChildPids, 0).

iter_pids(Indices, ChildPids, CurIndex) ->
  if
    length(ChildPids) > 0 ->
      [ChildPid | RestPids] = ChildPids,
      NewIndices = maps:put(ChildPid, CurIndex, Indices),
      iter_pids(NewIndices, RestPids, CurIndex + 1);
    true ->
      Indices
  end.

parallel_map(TargetFunction, Values) ->
  ProcessIds = lists:map(
    fun(X) ->
      spawn_and_eval(TargetFunction, X)
    end,
    Values),
  Indices = construct_indices(ProcessIds),
  Results = lists:map(
    fun(_) ->
      receive
        {ChildPid, Result} -> {maps:get(ChildPid, Indices), Result}
      end
    end,
    ProcessIds),
  SortedResults = lists:sort(Results),
  lists:map(
    fun({_Index, Value}) ->
      Value
    end,
    SortedResults).
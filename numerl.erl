-module(numerl).
-export([from_list/1, to_list/1, sum/2, mul/2, get_row/2, get_col/2, get_diag/1]).
-on_load(init/0).

init() ->
  ok = erlang:load_nif("./numerl_nif", 0).

%% converts list to matrix (vector) object
from_list(_List) ->
  exit(nif_library_not_loaded).

%% converts matrix (vector) object to list
to_list(_Obj) ->
  exit(nif_library_not_loaded).


%% sums two matrix (vector) objects
sum(_Obj1, _Obj2) ->
  exit(nif_library_not_loaded).


%% muptiplicates (matrix (vector) object and scalar) or (matrix object and vector object)
mul(_Obj1, _Obj2) ->
  exit(nif_library_not_loaded).

%% returns row
get_row(_Obj, _Index) ->
  exit(nif_library_not_loaded).

%% returns column
get_col(_Obj, _Index) ->
  exit(nif_library_not_loaded).

%% returns diagonal
get_diag(_Obj) ->
  exit(nif_library_not_loaded).
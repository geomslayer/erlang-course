-module(main).
-export([main/0]).


%% a little demonstraion:
main() ->
  %% basics:
  A = numerl:from_list([1.0, 2.0, 3.0]),
  io:fwrite("~w\n", [numerl:to_list(A)]),  %% [1.0, 2.0, 3.0]

  B = numerl:mul(A, 5.0),
  io:fwrite("~w\n", [numerl:to_list(B)]),  %% [5.0,10.0,15.0]

  C = numerl:sum(A, B),
  io:fwrite("~w\n", [numerl:to_list(C)]),  %% [6.0,12.0,18.0]

  %% two-dimention:
  D = numerl:from_list([[-4.0, 2.5], [5.5, 3.0], [-0.5, 2.0]]),
  io:fwrite("~w\n", [numerl:to_list(D)]),
  %% [[-4.0, 2.5],
  %%  [5.5,  3.0],
  %%  [-0.5, 2.0]]

  E = numerl:mul(D, -1.0),
  io:fwrite("~w\n", [numerl:to_list(E)]),
  %% [[4.0,  -2.5],
  %%  [-5.5, -3.0],
  %%  [0.5,  -2.0]]

  F = numerl:sum(D, E),
  io:fwrite("~w\n", [numerl:to_list(F)]),
  %% [[0.0, 0.0],
  %%  [0.0, 0.0],
  %%  [0.0, 0.0]]

  %% separate rows, cols, diags:
  G = numerl:get_row(E, 1),  %% not copied
  H = numerl:get_col(E, 0),
  I = numerl:get_diag(E),
  io:fwrite("~w\n", [numerl:to_list(G)]),  %% [-5.5,-3.0]
  io:fwrite("~w\n", [numerl:to_list(H)]),  %% [4.0,-5.5,0.5]
  io:fwrite("~w\n", [numerl:to_list(I)]),  %% [4.0,-3.0]

  %% Mat * Vec:
  K = numerl:mul(E, I),
  io:fwrite("~w\n", [numerl:to_list(K)]),  %% [23.5,-13.0,8.0]

  %% Vec * Scalar:

  L = numerl:mul(H, -2.0),
  io:fwrite("~w\n", [numerl:to_list(L)]).  %% [-8.0,11.0,-1.0]

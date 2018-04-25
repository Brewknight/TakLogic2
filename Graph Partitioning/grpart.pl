:- lib(ic).
:- lib(branch_and_bound).

grpart(N, D, P1, P2, Cost):-
    create_graph(N, D, Graph),
    length(Solution, N),
    Solution #:: 1..N,
    constrain(Solution, Graph, Cost),
    bb_min(search(Solution, 0, first_fail, indomain_middle, complete, []),
    Cost, bb_options{strategy:restart}),
    divide(Solution, P1, P2).

constrain(Solution, Graph, Cost):-
    alldifferent(Solution),
    divide(Solution, P1, P2),
    part(P1, P2, Graph, Cost).

divide(List, L1, L2):-
    length(List, L),
    Half is L - L // 2,
    length(L1, Half),
    append(L1, L2, List).

part([], _, _, 0).
part(F, S, G, N):-
    cmember(X, F),
    findall(Y, part(X, Y, S, G), Parts),
    delete(X, F, Xs),
    part(Xs, S, G, N1),
    length(Parts, L),
    N #= N1 + L.

part(X, Y, S, G):-
    \+is_list(X),
    cmember(Y, S),
    gcmember(X, Y, G).

cmember(X, [Y | _]):-
    X #= Y.
cmember(X, [Y | Ys]):-
    X #\= Y,
    cmember(X, Ys).

gcmember(X, Y, [A - B | T]):-
    (X #= A,
    Y #= B);
    (X #= B,
    Y #= A).
gcmember(X, Y, [_ | T]):-
    gcmember(X, Y, T).

computeCost(Parts, Cost):-
    findall(L, (member(P, Parts), length(P, L)), Ls),
    Cost #= sum(Ls).
    
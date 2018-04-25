:- lib(ic).
:- lib(branch_and_bound).

grpart(N, D, P1, P2, Cost):-
    create_graph(N, D, Graph),
    length(Solution, N),
    Solution #:: 1..N,
    constrain(Solution, Graph),
    search(Solution, 0, first_fail, indomain_middle, complete, []),
    divide(Solution, P1, P2),
    part(P1, P2, Graph, Parts),
    computeCost(Parts, Cost).

constrain(Solution, Graph):-
    alldifferent(Solution),
    divide(Solution, P1, P2),
    part(P1, P2, Graph, Parts).


divide(List, L1, L2):-
    length(List, L),
    Half is L - L // 2,
    length(L1, Half),
    append(L1, L2, List).

part([], _, _, []).
part([X | Xs], S, G, [Parts | Partss]):-
    %member(X, F),
    findall(Y, part(X, Y, S, G), Parts),
    %delete(X, F, Xs),
    part(Xs, S, G, Partss).

part(X, Y, S, G):-
    \+is_list(X),
    member(Y, S),
    (member(X - Y, G);
    member(Y - X, G)).

computeCost(Parts, Cost):-
    findall(L, (member(P, Parts), length(P, L)), Ls),
    summ(Ls, Cost).

summ([], 0).
summ([X | Xs], Sum):-
    summ(Xs, Sum1),
    Sum is Sum1 + X.
    
    
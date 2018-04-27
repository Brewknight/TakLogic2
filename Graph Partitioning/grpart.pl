:- lib(ic).
:- lib(branch_and_bound).

grpart(N, D, Graph, P1, P2, Cost):-
    create_graph(N, D, Graph),
    length(Solution, N),
    Solution #:: 1..N,
    constrain(Solution, Graph, Cost, P1, P2),
    bb_min(search(Solution, 0, first_fail, indomain_middle, complete, []),
    Cost, bb_options{strategy:restart}),
    divide(Solution, P1, P2).

constrain(Solution, Graph, Cost, P1, P2):-
    alldifferent(Solution),
    divide(Solution, P1, P2),
    partAll(P1, P2, Graph, Cost).

divide(List, L1, L2):-
    length(List, L),
    Half is L - L // 2,
    sublength(List, Half, L1, L2).

% Gives all possible combinations of members of GivLength length.
% Also returns Rest members
sublength(L, 0, [], L).
sublength(L, GivLength, Result, Rest):-
    length(L, Length),
    GivLength =< Length,

    member(X, L),
    delete(X, L, L1),

    GivLength1 is GivLength - 1,

    sublength(L1, GivLength1, Res1, Rest),
    append([X], Res1, Result).



partAll(First, Second, G, N):-
    partList(First, Second, G, G1),
    length(G, L1),
    length(G1, L2),
    N #= abs(L1 - L2).

partList([], _, G, G).
partList([X | Xs], Ys, G, G2):-
    part(X, Ys, G, G1),
    partList(Xs, Ys, G1, G2).

part(_, [], G, G).
part(_, _, [], []).
part(X, [Y | Ys], G, G2):-
    \+is_list(X),
    (gmember(X, Y, G), (delete(X - Y, G, G1) ; 
                       delete(Y - X, G, G1)) ),
    part(X, Ys, G1, G2).

part(X, [_Y | Ys], G, G2):-
    part(X, Ys, G, G2).

gmember(X, Y, [A - B | _]):-
    X #= A,
    Y #= B ;

    X #= B,
    Y #= A.

gmember(X, Y, [A - B | Rest]):-
    gmember(X, Y, Rest).
    

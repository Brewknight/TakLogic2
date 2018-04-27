:- lib(ic).
:- lib(branch_and_bound).

grpart(N, D, Graph, P1, P2, Cost):-
    create_graph(N, D, Graph),
    length(Solution, N),
    Solution #:: 1..N,
    constrain(Solution, Graph, Cost, P1, P2),
    bb_min(search(Solution, 0, first_fail, indomain_middle, complete, []),
    Cost, bb_options{strategy:restart}).

constrain(Solution, Graph, Cost, P1, P2):-
    alldifferent(Solution),
    divide(Solution, P1, P2),
    part(P1, P2, Graph, Cost).

sublength(List, 0, [], List).
sublength(List, GivLength, Result, Rest):-
    length(List, L),
    GivLength =< L,

    member(X, List),
    delete(X, List, List1),
    GivLength1 is GivLength - 1,

    sublength(List1, GivLength1, Res1, Rest),
    append([X], Res1, Result).

divide(List, L1, L2):-
    length(List, L),
    Half is L - L // 2,
    sublength(List, Half, L1, L2).

connect([], _, []).
connect(_, [], []).
connect([X | Xs], [Y | Ys], [X - Y | Result]):-
    connect([X], Ys, Res1),
    connect(Xs, [Y | Ys], Res2),
    append(Res1, Res2, Result).

part(P1, P2, G, Cost):-
    connect(P1, P2, C),!,
    computeCost(C, G, 0, Cost).

computeCost([], _, N, Cost):-
    Cost #= N.
computeCost([Head | Tail], G, N, Cost):-
    (notgmember(Head, G),!, 
    computeCost(Tail, G, N, Cost));
    (N1 #= N + 1,
    computeCost(Tail, G, N1, Cost)).

notgmember(X - Y, [A - B | _]):-
    (X \= A,
    Y \= B);
    (X \= B,
    Y \= A).
notgmember(X, [_ | T]):-
    notgmember(X, T).

% subtract([Head | Tail], G2, Result):-
%     delete(Head, G2, Rest),
%     subtract(Tail, G2, Rest1),
%     append(Rest, Rest1, Result).
    
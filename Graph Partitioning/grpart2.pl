:- lib(ic).
:- lib(branch_and_bound).

grpart(N, D, Graph, P1, P2, Cost):-
    create_graph(N, D, Graph),
    length(Solution, N),
    Solution #:: 1..N,
    constrain(Solution, Graph, CostList, P1, P2),
    Cost #= sum(CostList),!,
    bb_min(search(Solution, 0, first_fail, indomain_middle, complete, []),
    Cost, bb_options{strategy:restart}).

constrain(Solution, Graph, CostList, P1, P2):-
    alldifferent(Solution),
    divide(Solution, P1, P2),
    length(Graph, L),
    Cost #:: 0..L,
    part(P1, P2, Graph, CostList).

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

part(P1, P2, G, CostList):-
    connect(P1, P2, C),
    computeCost(C, G, CostList).

computeCost(_, [], []).
computeCost([], _, []).
computeCost([Head | Tail], G, [0 | Fs]):-
    gmember(Head, G, 0, X),
    write("HI"),
    computeCost(Tail, G, Fs).
computeCost([X - Y | Tail], G, [1 | Fs]):-
    gmember(X - Y, G, 1, A - B),
    delete(A - B, G, G1),!,
    computeCost(Tail, G1, Fs).



gmember(X - Y, [A - B | _], Ret, A - B):-
    F #= (X #= A and Y #= B) or (X #= B and Y #= A),
    F = Ret.
gmember(X, [_ | T], F, R):-
    gmember(X, T, F, R).


% subtract([Head | Tail], G2, Result):-
%     delete(Head, G2, Rest),
%     subtract(Tail, G2, Rest1),
%     append(Rest, Rest1, Result).
    
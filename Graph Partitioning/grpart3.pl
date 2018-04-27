:- lib(ic).
:- lib(branch_and_bound).

grpart(N, D, Graph, P1, P2, Cost):-
    create_graph(N, D, Graph),
    length(Solution, N),
    Solution #:: 1..N,
    constrain(Solution, Graph, Cost),
    bb_min(search(Solution, 0, first_fail, indomain_middle, complete, []),
    Cost, _),
    divide(Solution, P1, P2).

constrain(Solution, Graph, Cost):-
    alldifferent(Solution),
    divide(Solution, P1, P2),
    computeCost(Graph, P1, P2, 0, Cost).

divide(List, L1, L2):-
    length(List, L),
    Half is L - L // 2,
    length(L1, Half),
    append(L1, L2, List).

computeCost([], _, _, N, Cost):-
    Cost #= N.
computeCost([A - B | T], P1, P2, N, Cost):-

    ( (checkdom(P2),
    take(P2, A),
    take(P2, B);
    checkdom(P2),
    take(P1, A),
    take(P1, B) ),
    
    computeCost(T, P1, P2, N, Cost) );

    ( (checkdom(P1),
    checkdom(P2),
    take(P1, B),
    take(P2, A);
    checkdom(P1),
    checkdom(P2),
    take(P1, A),
    take(P2, B) ),

    N1 #= N + 1,
    computeCost(T, P1, P2, N1, Cost) ).

take([], _).
take([H | T], X):-
    H #\= X,
    take(T, X).

checkdom([H | T]):-
    get_domain_size(H, Size),
    length([H | T], L),
    Size >= L.
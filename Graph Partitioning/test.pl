:- lib(ic).
:- lib(branch_and_bound).

go(X, Y, Cost):-
    length(S, 3),
    S #:: 1..3,
    alldifferent(S),
    divide(S, X, Y),
    length(C, 3),
    C #:: 0..1, 
    G = [1 - 2, 1 - 3, 2 - 3],

    costFunction(G, X, Y, C),
    Cost #= sum(C),

    bb_min(search(S, 0, first_fail, indomain_middle, complete, []),
    Cost, bb_options{strategy:restart}).

divide(List, L1, L2):-
    length(List, L),
    Half is L - L // 2,
    length(L1, Half),
    append(L1, L2, List).

costFunction([], _, _, []).
costFunction([H | GT], P1, P2, [C | Cs]):-
    costFunction(H, P1, P2, C),
    costFunction(GT, P1, P2, Cs).

costFunction(A - B, P1, P2, C):-
    ( (negcheck(P1, A) and negcheck(P1, B)) => (C #\= 1) ),
    ( (negcheck(P2, A) and negcheck(P2, B)) => (C #\= 1) ),
    ( (negcheck(P1, A) and negcheck(P2, B)) => (C #\= 0) ),
    ( (negcheck(P2, A) and negcheck(P1, B)) => (C #\= 0) ).
    

negcheck([H], X, Bool):-
    Bool #= (H #\= X).
negcheck([H | T], X, Bool):-
    Bool #= (H #\= X and negcheck(T, X)).


findInDom(X, [H], 0):-
    \+is_in_domain(X, H).
findInDom(X, [H], 1):-
    is_in_domain(X, H).
findInDom(X, [H | T], Bool):-
    is_in_domain(X, H),
    findInDom(X, T, Bool).

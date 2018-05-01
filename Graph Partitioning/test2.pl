:- lib(ic).
:- lib(branch_and_bound).

go(X, Y, Cs):-
    
    length(S, 3),
    S #:: 1..3,
    
    divide(S, X, Y),

    alldifferent(S),

    N #= 0,

    G = [1 - 2, 1 - 3, 2 - 3],

    length(X, L),
    length(Cs, L),
    length(G, GL),
    Cs #:: 0..GL,

    iterVars(X, Y, G, N, Cs),

    search(S, 0, most_constrained, indomain, complete, []).
     

iterVars([], _, _, _, []).
iterVars([H | T], OtherVars, G, N, [C | Cs]):-
    iterGraph(G, H, [H | T], OtherVars, N, C),
    iterVars(T, OtherVars, G, N, Cs).


iterGraph([], _, _, _, N, C):-
    C #= N.
iterGraph([A - B | T], X, Xs, OtherVars, N, C):-
    ( ( (X #= A and negcheck(OtherVars, B)) or (X #= B and negcheck(OtherVars, A)) ) => (N1 #= N) ),
    ( ( (X #= A and negcheck(Xs, B)) or (X #= B and negcheck(Xs, A)) ) => (N1 #= N + 1) ),
    iterGraph(T, X, Xs, OtherVars, N1, C).




divide(List, L1, L2):-
    length(List, L),
    Half is L - L // 2,
    length(L1, Half),
    append(L1, L2, List).


% Checks for negation in whole list.
negcheck([H], X, Bool):-
        Bool #= (H #\= X).
    negcheck([H | T], X, Bool):-
        Bool #= (H #\= X and negcheck(T, X)).
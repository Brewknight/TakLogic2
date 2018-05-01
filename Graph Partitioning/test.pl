:- lib(ic).
:- lib(branch_and_bound).

go(X, Y, C):-

    length(X, 2),
    length(Y, 2),
    X #:: 1..4,
    Y #:: 1..4,
    
    append(X, Y, S),
    alldifferent(S),

    C #:: 0..1,
    
    ( (neg negcheck(X, 1, 2)) => (C #= 0) ) or (C #= 1),
    ( (neg negcheck(Y, 1, 2)) => (C #= 0) ),
    

    % ((poscheck(X, 1, 2) or poscheck(Y, 1, 2)) => (C #= 1)),
    % ( ( (neg poscheck(X, 1, 2)) and (neg poscheck(Y, 1, 2)) ) => (C #= 0)),
    search(S, 0, most_constrained, indomain, complete, []).


% Checks for negation in whole list.
negcheck([H], X, Bool):-
    Bool #= (H #\= X).
negcheck([H | T], X, Bool):-
    Bool #= (H #\= X and negcheck(T, X)).

negcheck(List, A, B, Bool):-
    Bool #= (negcheck(List, A) or negcheck(List, B)).


poscheck([H], X, Bool):-
    Bool #= (H #= X).
poscheck([H | T], X, Bool):-
    Bool #= (H #= X or poscheck(T, X)).


poscheck(List, A, B, Bool):-
    Bool #= check(List, A, B, 1).

check(List, A, B, N, Bool):-
    length(List, N),
    nth(N, List, Elem),
    delete(Elem, List, List1),
    Bool #= (Elem #= A and poscheck(List1, B)).
check(List, A, B, N, Bool):-
    length(List, L),
    N < L,
    nth(N, List, Elem),
    delete(Elem, List, List1),
    N1 is N + 1,
    Bool #= ((Elem #= A and poscheck(List1, B)) or check(List, A, B, N1)).

nth(1, [Elem| _], Elem).
nth(N, [_| Elems], Elem) :-
    N \= 1,
    N1 is N - 1,
    nth(N1, Elems, Elem).
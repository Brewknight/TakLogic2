go(Y):-
    length(Y, 5), 
    Y  #:: [0, 1],
    openEnsure(Y, 1),
    search(Y, 0, most_constrained, indomain, complete, []).


openEnsure([Y], Bool):-
    Bool #= (Y #= 1).
openEnsure([Y | Ys], Bool):-
    Bool #= ((Y #= 1) or (openEnsure(Ys))).
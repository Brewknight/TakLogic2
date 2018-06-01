:- lib(ic).
:- lib(branch_and_bound).


go(I, Sublist):-
    length(I, 9),
    I #:: 1..9, 
    alldifferent(I),
    Min #= min(I),
    Max #= max(I),
    
    subListMinMax(Min, Max, I, Sublist, 1, 1),

    %append(I, [Index], Ni).

    search(I, 0, first_fail, indomain, complete, []).

icappend(L1, L2, Res, Bool):-
    append(L1, L2, Res),
    Bool #= 1.

findCosts(_Min, _Max, [], [], _N).
findCosts(Min, Max, [I | Is], [Bool | Bools], N):-
    N1 is N + 1,
    Bool #= (N #>= Min and N #<= Max and I #\= 0),
    findCosts(Min, Max, Is, Bools, N1).
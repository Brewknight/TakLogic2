:- lib(ic).
:- lib(branch_and_bound).

nth(1, [Elem| _], Elem).
nth(N, [_| Elems], Elem) :-
    N \= 1,
    N1 is N - 1,
    nth(N1, Elems, Elem).

go(N, Solution, Cost):-
    N1 is N - 1,
    getNCosts(N1, NCosts),
    flatten(NCosts, Costs),
    length(VarCosts, N),
    length(Solution, N),
    Solution #:: 1..N,
    alldifferent(Solution),
    VarCosts #:: 0..1.0Inf,
    constrain(Solution, Solution, Costs, VarCosts),
    Cost #= sum(VarCosts),

    bb_min(search(Solution, 0, first_fail, indomain, complete, []),
    Cost, bb_options{strategy:dichotomic, delta:1, report_failure:1}).


constrain([N], [M | Tail], Costs, [VarCost]):-
    length([M | Tail], L),
    findIndex(N, M, L, Smaller),
    findIndex(M, N, L, Bigger),
    (N #< M) => (Index #= Smaller),
    (N #> M) => (Index #= Bigger),
    element(Index, Costs, VarCost).
constrain([N, M | Tail], Solution, Costs, [VarCost | VarCosts]):-
    length(Solution, L),
    findIndex(N, M, L, Smaller),
    findIndex(M, N, L, Bigger),
    (N #< M) => (Index #= Smaller),
    (N #> M) => (Index #= Bigger),
    element(Index, Costs, VarCost),
    constrain([M | Tail], Solution, Costs, VarCosts).


findIndex(N, M, L, Index):-
    % Make it integer
    Index #:: 1..1.0Inf,
    Index $= (1 / 2) * (N - 1) * (2*L - N) + (M - N).


% Cost retrieval
getNCosts(0, []).
getNCosts(N, Costs):-
    costs(AllCosts),
    length(AllCosts, L),
    L1 is L - N,
    length(Costs, N),
    length(Rest, L1),
    append(Rest, Costs, AllCosts).
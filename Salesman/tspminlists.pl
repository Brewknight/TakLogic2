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

    search(Solution, 0, first_fail, indomain, complete, []).
    %Cost, bb_options{strategy:dichotomic, delta:1, report_failure:1}).


constrain([N], [M | Tail], Costs, [VarCost]):-
    M #\= N,
    length([M | Tail], L),
    findIndex(N, D, L, Smaller),
    findIndex(D, N, L, Bigger),
    (N #< D) => (Index #= Smaller),
    (N #> D) => (Index #= Bigger),
    element(Index, Costs, VarCost).
    % findCostsAsList([M | Tail], N, M, Costs, CostList, Domain),
    % VarCost #= min(CostList),
    % findIndexByValue(CostList, VarCost, 1, Index, 1),
    % element(Index, Domain, M).
constrain([N, M | Tail], Solution, Costs, [VarCost | VarCosts]):-
    M #\= N,
    findCostsAsList(Solution, N, M, Costs, CostList, Domain),
    VarCost #= min(CostList),
    findIndexByValue(CostList, VarCost, 1, Index, 1),
    element(Index, Domain, M),
    constrain([M | Tail], Solution, Costs, VarCosts).

findCostsAsList(Solution, N, M, Costs, CostList, Domain):-
    get_domain_as_list(M, Domain),
    findCostList(Solution, N, Domain, Costs, CostList).

findCostList(_, _, [], _, []).
findCostList(Solution, N, [D | Ds], Costs, [C | Cs]):-
    length(Solution, L),
    findIndex(N, D, L, Smaller),
    findIndex(D, N, L, Bigger),
    (N #< D) => (Index #= Smaller),
    (N #> D) => (Index #= Bigger),
    element(Index, Costs, C),
    findCostList(Solution, N, Ds, Costs, Cs).

findIndex(N, M, L, Index):-
    % Make it integer
    Index #:: 1..1.0Inf,
    Index $= (1 / 2) * (N - 1) * (2*L - N) + (M - N).


% Finds in which index Value lies within a list of ic variables
findIndexByValue([Y], Value, N, Index, Bool):-
    Bool #= ((Y #= Value) and (Index #= N)).
findIndexByValue([Y | Ys], Value, N, Index, Bool):-
    N1 is N + 1,
    Bool #=  ( ((Y #= Value) and (Index #= N)) or findIndexByValue(Ys, Value, N1, Index) ).

% Cost retrieval
getNCosts(0, []).
getNCosts(N, Costs):-
    costs(AllCosts),
    length(AllCosts, L),
    L1 is L - N,
    length(Costs, N),
    length(Rest, L1),
    append(Rest, Costs, AllCosts).
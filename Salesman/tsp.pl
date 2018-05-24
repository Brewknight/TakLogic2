:- lib(ic).
:- lib(branch_and_bound).

nth(1, [Elem| _], Elem).
nth(N, [_| Elems], Elem) :-
    N \= 1,
    N1 is N - 1,
    nth(N1, Elems, Elem).

myelement(I, List, Value):-
    N #= 1,
    elementList(I, N, List, Value, 1).

elementList(_, _, [], _, 0).
elementList(I, N, [Head | Tail], Value, 1):-
    (N #= I and assignList(Value, Head)) 
    or (N #\= I and N1 #= N + 1 and elementList(I, N1, Tail, Value)).

% constrains Value to be List
assignList([], [], 1).
assignList([VHead | VTail], [Head | Tail], 1):-
    VHead #= Head,
    assignList(VTail, Tail).


go(N, Solution, Cost):-
    N1 is N - 1,
    getNCosts(N1, NCosts),
    length(Costs, N),
    % Make a list for each city - How much it costs to get to any other city.
    % The Cost to get from a city to itself is 0.
    % This case will never exist, because we have alldifferent working for us.
    % Just want to be safe with indexing
    getAllCosts(NCosts, 1, Costs),
    % An integer list 1..N, NOT a domain variable list
    makeCityList(N, 0, CityList),

    length(Solution, N),
    Solution #:: 1..N,
    alldifferent(Solution),
    length(VarCosts, N),
    VarCosts #:: 0..1.0Inf,
    constrain(Solution, Solution, Costs, CityList, VarCosts),
    Cost #= sum(VarCosts),

    search(Solution, 0, first_fail, indomain, complete, []).

constrain([_A], [B | _Tail], Costs, CityList, [VarCost]):-
    length(CityList, N),
    I #:: 1..N,
    City #:: 1..N,
    element(I, CityList, City),
    nth(City, Costs, AList),
    element(B, AList, VarCost).

constrain([_A, B | Tail], Solution, Costs, CityList, [VarCost | VarCosts]):-
    length(CityList, N),
    I #:: 1..N,
    City #:: 1..N,
    element(I, CityList, City),
    nth(City, Costs, AList),
    element(B, AList, VarCost),
    constrain([B | Tail], Solution, Costs, CityList, VarCosts).

makeCityList(N, M, []):-
    M > N.
makeCityList(N, M, [M | Cities]):-
    M =< N,
    M1 is M + 1,
    makeCityList(N, M1, Cities).


% Cost retrieval
getNCosts(0, []).
getNCosts(N, Costs):-
    costs(AllCosts),
    length(AllCosts, L),
    L1 is L - N,
    length(Costs, N),
    length(Rest, L1),
    append(Rest, Costs, AllCosts).

getAllCosts(_, _, []).
getAllCosts(NCosts, N, [Costs | Costss]):-
    length(NCosts, L),
    L1 is L + 1,
    length(Costs, L1),

    getMCosts(N, 1, NCosts, Costs),
    N1 is N + 1,
    getAllCosts(NCosts, N1, Costss).

getMCosts(_, _, _, []).
getMCosts(N, M, NCosts, [Cost | Costs]):-
    getCost(N, M, NCosts, Cost),
    M1 is M + 1,
    getMCosts(N, M1, NCosts, Costs).

% Cost to get from N City to M City
getCost(N, M, _, 0):-
    N == M.
getCost(N, M, NCosts, Cost):-
    N < M,
    nth(N, NCosts, List),
    K is M - N,
    nth(K, List, Cost).

getCost(N, M, NCosts, Cost):-
    N > M,
    nth(M, NCosts, List),
    K is N - M,
    nth(K, List, Cost).


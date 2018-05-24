:- lib(ic).
:- lib(branch_and_bound).

warehouses(N1, M1,  YesNoLocs,  CustServs,  Cost):-
    getWarehouses(N1, 1, FixedCosts),
    getCustomers(N1, M1, 1, VarCosts),!,

    length(FixedCosts, W),
    length(VarCosts, C),
    length(YesNoLocs, W),
    length(CustServs, C),

    YesNoLocs #:: [0, 1],
    CustServs #:: 1..W,
    constrain(YesNoLocs, FixedCosts, CustServs, VarCosts, W, C, Cost),
    append(CustServs, YesNoLocs, Solution),
    bb_min(search(Solution, 0, first_fail, indomain, complete, []),
    Cost, bb_options{strategy:continue, delta:1, report_failure:1}).

constrain(YesNoLocs, FixedCosts, CustServs, VarCosts, N, M, Cost):-
% Create Templates for WareCosts and CustCosts : 
    length(WareCosts, N),
    % List for each warehouse, either 0 or FixedCost
    constrainWareCosts(WareCosts, FixedCosts),
    
    length(CustCosts, M),
    % List for each customer, either 0 or VarCost
    constrainCustCosts(CustCosts, VarCosts),

    % Both must be 0 or both must be not 0
    constrainAllCustWares(CustCosts, WareCosts),

    % Open Warehouse based on CustCosts and WareCosts
    checkAllWares(CustCosts, YesNoLocs),
    rev(YesNoLocs, RevYesNoLocs),

    length(MinCosts, M),
    % Non zero mins and indices of non zero mins of cust costs is our cost and serv
    findNonZeroMins(CustCosts, RevYesNoLocs, MinCosts, CustServs),
    CCost #= sum(MinCosts),
    WCost #= sum(WareCosts),
    Cost #= CCost + WCost.

% Reverses a list of binary ic variables
rev([], []).
rev([Y | Ys], [Z | Zs]):-
    Z #= (Y #\= 1),
    rev(Ys, Zs).

% Multiplies each element of list of ic variables by value.
multiply([], [], _).
multiply([Y | Ys], [Z | Zs], Value):-
    Z #= Y * Value,
    multiply(Ys, Zs, Value).

% Adds 2 lists of ic variables Xs, Ys, with the same length, creating Zs where each Z = X + Y
add([], [], []).
add([X | Xs], [Y | Ys], [Z | Zs]):-
    Z #= X + Y,
    add(Xs, Ys, Zs).

findNonZeroMins([], _, [], []).
findNonZeroMins([CostList | CostLists], RevYesNoLocs, [Cost | Costs], [Serv | Servs]):-
    length(RevYesNoLocs, L),
    length(Ys, L),
    Ys #:: [0, 2000000],
    multiply(RevYesNoLocs, Ys, 2000000),
    add(CostList, Ys, Zs),
    Cost #= min(Zs),
    findIndex(CostList, Cost, 1, Serv, 1),
    findNonZeroMins(CostLists, RevYesNoLocs, Costs, Servs).

% Finds in which index Value lies within a list of ic variables
findIndex([Y], Value, N, Index, Bool):-
    Bool #= ((Y #= Value) and (Index #= N)).
findIndex([Y | Ys], Value, N, Index, Bool):-
    N1 is N + 1,
    Bool #=  ( ((Y #= Value) and (Index #= N)) or findIndex(Ys, Value, N1, Index) ).


constrainAllCustWares([], _).
constrainAllCustWares([CustCost | CustCosts], WareCosts):-
    constrainCustWares(CustCost, WareCosts),
    constrainAllCustWares(CustCosts, WareCosts).
% Both CustCost and WareCost must be either 0 or not 0
constrainCustWares([], []).
constrainCustWares([C | Cs], [W | Ws]):-
    ((C #= 0 and W #= 0) or (C #\= 0 and W #\= 0)),
    constrainCustWares(Cs, Ws).

checkAllWares([], _).
checkAllWares([CustCost | CustCosts], YesNoLocs):-
    checkWares(CustCost, YesNoLocs),
    checkAllWares(CustCosts, YesNoLocs).

checkWares([], []).    
checkWares([C | Cs], [Y | Ys]):-
    Y #= (C #\= 0),
    checkWares(Cs, Ys).


constrainCustCosts([], []).
constrainCustCosts([CustCost | CustCosts], [VarCost | VarCosts]):-
    length(VarCost, L),
    length(CustCost, L),
    constrainCustCost(CustCost, VarCost),
    constrainCustCosts(CustCosts, VarCosts).
% Each CustCost is either 0 or VarCost
constrainCustCost([], []).
constrainCustCost([C | Cs], [V | Vs]):-
    C #:: [0, V],
    constrainCustCost(Cs, Vs).

% Each WareCost is either 0 or FixedCost
constrainWareCosts([], []).
constrainWareCosts([WareCost | WareCosts], [FixedCost | FixedCosts]):-
    WareCost #:: [0, FixedCost],
    constrainWareCosts(WareCosts, FixedCosts).



% Pull wanted data
getWarehouses(0, _, AllHouses):-
    fixedcosts(AllHouses).
getWarehouses(WN, N, []):-
    N > WN.
getWarehouses(WN, N, [House | Houses]):-
    N =< WN,
    fixedcosts(AllHouses),
    nth(N, AllHouses, House),
    N1 is N + 1,
    getWarehouses(WN, N1, Houses).


getCustomers(WN, 0, N, Customers):-
    varcosts(AllCustomers),
    length(AllCustomers, L),
    getCustomers(WN, L, N, Customers).
getCustomers(_, CM, N, []):-
    N > CM.
getCustomers(WN, CM, N, [Customer | Customers]):-
    N =< CM,
    varcosts(AllCustomers),
    nth(N, AllCustomers, AllCosts),
    getCustomer(AllCosts, WN, 1, Customer),
    N1 is N + 1,
    getCustomers(WN, CM , N1, Customers).


getCustomer(AllCosts, 0, _, AllCosts).
getCustomer(_, WN, N, []):-
    N > WN.
getCustomer(AllCosts, WN, N, [VarCost | VarCosts]):-
    N =< WN,
    nth(N, AllCosts, VarCost),
    N1 is N + 1,
    getCustomer(AllCosts, WN, N1, VarCosts).



% nth(1, [Elem| _], Elem).
% nth(N, [_| Elems], Elem) :-
%     N \= 1,
%     N1 is N - 1,
%     nth(N1, Elems, Elem).

nth(I, [Elem | _], Elem):-
    I #= 1.
nth(I, [_ | Elems], Elem):-
    I #\= 1,
    I1 #= I + 1,
    nth(I1, Elems, Elem).
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
    constrain(YesNoLocs, FixedCosts, CustServs, VarCosts, W, C, Cost),!,
    append(YesNoLocs, CustServs, Solution),
    bb_min(search(Solution, 0, first_fail, indomain, complete, []),
    Cost, bb_options{strategy:continue, delta:1, report_failure:1}).

constrain(YesNoLocs, FixedCosts, CustServs, VarCosts, N, M, Cost):-
    length(WareCosts, N),
    % List for each warehouse, either 0 or FixedCost
    constrainWareCosts(WareCosts, FixedCosts),
    
    length(CustCosts, M),
    % List for each customer, either 0 or VarCost
    constrainCustCosts(CustCosts, VarCosts),

    % Open Warehouse based on CustCosts and WareCosts
    checkAllWares(CustCosts, WareCosts, YesNoLocs),

    length(MinCosts, M),
    % Non zero mins of cust costs is our cost and serv
    findNonZeroMins(CustCosts, YesNoLocs, MinCosts, CustServs),
    CCost #= sum(MinCosts),
    WCost #= sum(WareCosts),
    Cost #= CCost + WCost.

rev([], []).
rev([Y | Ys], [Z | Zs]):-
    (Y #= 0) => (Z #= 2000000),
    (Y #= 1) => (Z #= 0),
    rev(Ys, Zs).

add([], [], []).
add([X | Xs], [Y | Ys], [Z | Zs]):-
    Z #= X + Y,
    add(Xs, Ys, Zs).

findNonZeroMins([], _, [], []).
findNonZeroMins([CostList | CostLists], YesNoLocs, [Cost | Costs], [Serv | Servs]):-
    length(YesNoLocs, L),
    length(Ys, L),
    Ys #:: [0, 2000000],
    rev(YesNoLocs, Ys),
    add(CostList, Ys, Zs),
    Cost #= min(Zs),
    findIndex(CostList, Cost, 1, Serv, 1),
    findNonZeroMins(CostLists, YesNoLocs, Costs, Servs).


findIndex([Y], Min, N, Index, Bool):-
    Bool #= ((Y #= Min) and (Index #= N)).
findIndex([Y | Ys], Min, N, Index, Bool):-
    N1 is N + 1,
    Bool #=  ( ((Y #= Min) and (Index #= N)) or findIndex(Ys, Min, N1, Index) ).


constrainAllCustWares([], _).
constrainAllCustWares([CustCost | CustCosts], WareCosts):-
    constrainCustWares(CustCost, WareCosts, 1),
    constrainAllCustWares(CustCosts, WareCosts).

constrainCustWares([C], [W], Bool):-
    Bool #= (C #= 0 and W #= 0) or (C #\= 0 and W #\= 0).
constrainCustWares([C | Cs], [W | Ws], Bool):-
    Bool #= ((C #= 0 and W #= 0) or (C #\= 0 and W #\= 0)) or
    constrainCustWares(Cs, Ws).

checkAllWares([], _, _).
checkAllWares([CustCost | CustCosts], WareCosts, YesNoLocs):-
    checkWares(CustCost, WareCosts, YesNoLocs),
    checkAllWares(CustCosts, WareCosts, YesNoLocs).

checkWares([], [], []).    
checkWares([C | Cs], [W | Ws], [Y | Ys]):-
    %Y #= (C #\= 0 and W #\= 0),
    (Y #= 0) => (C #= 0 and W #= 0),
    (Y #= 1) => (C #\= 0 and W #\= 0),
    checkWares(Cs, Ws, Ys).


constrainCustCosts([], []).
constrainCustCosts([CustCost | CustCosts], [VarCost | VarCosts]):-
    % Each CustCost is either 0 or VarCost
    length(VarCost, L),
    length(CustCost, L),
    constrainCustCost(CustCost, VarCost),
    constrainCustCosts(CustCosts, VarCosts).

constrainCustCost([], []).
constrainCustCost([C | Cs], [V | Vs]):-
    C #:: [0, V],
    constrainCustCost(Cs, Vs).

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

nth(1, [Elem| _], Elem).
nth(N, [_| Elems], Elem) :-
    N \= 1,
    N1 is N - 1,
    nth(N1, Elems, Elem).
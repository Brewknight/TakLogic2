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
    append(YesNoLocs, CustServs, Solution),
    search(Solution, 0, most_constrained, indomain, complete, []).

constrain(YesNoLocs, FixedCosts, CustServs, VarCosts, N, M, Cost):-
    length(WareCosts, N),
    WareCosts #:: 0..1.0Inf,
    constrainWares(YesNoLocs, FixedCosts, WareCosts),
    WareCost #= sum(WareCosts),
    openEnsure(YesNoLocs),

    length(CustCosts, M),
    CustCosts #:: 0..1.0Inf,
    constrainCusts(CustServs, VarCosts, YesNoLocs, CustCosts),
    CustCost #= sum(CustCosts),
    Cost #= WareCost + CustCost.

constrainCusts([], _, _, []).
constrainCusts([Serv | Servs], [VarCost | VarCosts], YesNoLocs, [Cost | Costs]):-
    constrainCust(1, VarCost, YesNoLocs, Cost, Serv),
    constrainCusts(Servs, VarCosts, YesNoLocs, Costs).


constrainCust(_, [], [], _, _).
constrainCust(I, [C | Cs], [Y | Ys], Cost, Serv):-
    (Y #= 1) => (Cost #= C and Serv #= I),
    I1 is I + 1,
    constrainCust(I1, Cs, Ys, Cost, Serv).


constrainWares([], [], []).
constrainWares([Y | Ys], [W | Ws], [C | Cs]):-
    (Y #= 0) => (C #= 0),
    (Y #= 1) => (C #= W),
    constrainWares(Ys, Ws, Cs).
    
% One warehouse at least should be open
openEnsure([Y]):-
    Y #= 1.
openEnsure([Y | Ys]):-
    Bool #= ((Y #= 1) or openEnsure(Ys)),
    Bool = 1.
openEnsure([Y | Ys], Bool):-
    Bool #= ((Y #= 1) or openEnsure(Ys)).
openEnsure([], 0).


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
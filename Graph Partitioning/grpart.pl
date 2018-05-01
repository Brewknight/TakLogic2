:- lib(ic).
:- lib(branch_and_bound).

grpart(N, D, P1, P2, Cost):-
    create_graph(N, D, Graph),
    length(Solution, N),
    Solution #:: 1..N,
    constrain(Solution, Graph, Costs),
    % Count the "1"s in Costs list
    % This number is how many edges have been cut
    Cost #= sum(Costs),
    bb_min(search(Solution, 0, most_constrained, indomain, complete, []),
    Cost, bb_options{report_failure:1, timeout:100}),
    divide(Solution, P1, P2).

constrain(Solution, Graph, Costs):-
    alldifferent(Solution),
    divide(Solution, P1, P2),
    % Costs is a list of bools
    % One element for each edge in graph
    % 1 stands for cut edge, 0 stands for uncut edge
    length(Graph, G),
    length(Costs, G),
    Costs #:: 0..1,
    costFunction(Graph, P1, P2, Costs).

% Takes a list and breaks it in 2
% If length(List) is an odd number, length(P1) > length(P2)
divide(List, L1, L2):-
    length(List, L),
    Half is L - L // 2,
    length(L1, Half),
    append(L1, L2, List).


costFunction([], _, _, []).
costFunction([H | GT], P1, P2, [C | Cs]):-
    costFunction(H, P1, P2, C),
    costFunction(GT, P1, P2, Cs).

costFunction(A - B, P1, P2, C):-
    % If A,B are both in one group, then edge A - B is not cut. Cost = 0.
    % ( ( (neg negcheck(P1, A, B)) or (neg negcheck(P2, A, B)) ) => (C #= 0)  or C #= 1),

    % ( ( neg ( (neg negcheck(P1, A, B)) or (neg negcheck(P2, A, B)) ) ) => (C #= 1) ),

    T1 #= ((neg negcheck(P1, A)) and (negcheck(P1, B)) ),
    T2 #= ((neg negcheck(P1, B)) and (negcheck(P1, A)) ),
    T3 #= ((neg negcheck(P2, A)) and (negcheck(P2, B)) ),
    T4 #= ((neg negcheck(P2, B)) and (negcheck(P2, A)) ),

    ( (T1 or T2 or T3 or T4) => (C #= 1)),
    ( (neg T1 and neg T2 and neg T3 and neg T4) => (C #= 0)).
    
% Checks for negation in whole list.
negcheck([H], X, Bool):-
    Bool #= (H #\= X).
negcheck([H | T], X, Bool):-
    Bool #= (H #\= X and negcheck(T, X)).

negcheck(List, A, B, Bool):-
    Bool #= (negcheck(List, A) or negcheck(List, B)).
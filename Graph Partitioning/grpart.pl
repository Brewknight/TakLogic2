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
    bb_min(search(Solution, 0, most_constrained, indomain_min, complete, []),
    Cost, bb_options{strategy:continue, timeout:100}),
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
    % If A,B are both not in P1, then they are both in P2, and edge A - B is not cut. Cost = 0.
    ( (negcheck(P1, A) and negcheck(P1, B)) => (C #= 0) ),
    % If A,B are both not in P2, then they are both in P1, and edge A - B is not cut. Cost = 0.
    ( (negcheck(P2, A) and negcheck(P2, B)) => (C #= 0) ),
    % If A is not in P1 and B is not in P2, then A is in P2 and B is in P1, and edge A - B is cut. Cost = 1.
    ( (negcheck(P1, A) and negcheck(P2, B)) => (C #= 1) ),
    % If A is not in P2 and B is not in P2, then A is in P2 and B is in P1, and edge A - B is cut. Cost = 1.
    ( (negcheck(P2, A) and negcheck(P1, B)) => (C #= 1) ).
    
% Checks for negation in whole list.
negcheck([H], X, Bool):-
    Bool #= (H #\= X).
negcheck([H | T], X, Bool):-
    Bool #= (H #\= X and negcheck(T, X)).
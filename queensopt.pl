:- lib(ic).                % Φορτώστε τη βιβλιοθήκη ic
:- lib(branch_and_bound).  % Φορτώστε τη βιβλιοθήκη branch_and_bound

queensopt(N, Solution, Cost) :-
   length(Solution, N),    % Δημιουργία προτύπου λύσης
   Solution #:: 1..N,      % Πεδίο μεταβλητών της λύσης
   constrain(Solution, CostList, 1),  % Περιορισμοί και λίστα κοστών
   Cost #= max(CostList),  % Δήλωση κόστους λύσης
   bb_min(search(Solution, 0, first_fail, indomain_middle, complete, []),
          Cost, bb_options{strategy:restart}).   % Διακλάδωσε και φράξε

constrain([], [], _).
constrain([X|Xs], [abs(2*K-X)|CostList], K) :-
   noattack(X, Xs, 1),     % Μη απειλή βασίλισσας με τις επόμενες
   K1 is K + 1,
   constrain(Xs, CostList, K1).

noattack(_, [], _).
noattack(X, [Y|Ys], M) :-
   X #\= Y,                % Βασίλισσες όχι στην ίδια γραμμή
   X #\= Y-M,              % Βασίλισσες όχι στην ίδια ανιούσα διαγώνιο
   X #\= Y+M,              % Βασίλισσες όχι στην ίδια κατιούσα διαγώνιο
   M1 is M+1,
   noattack(X, Ys, M1).

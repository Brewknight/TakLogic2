:- lib(ic).
:- lib(branch_and_bound).

rehearsal(Sequence, WaitTime, TimeOut):-
    durations(FixedDurations),

    length(FixedDurations, Pieces),
    length(Sequence, Pieces),
    Sequence #:: 1..Pieces,
    alldifferent(Sequence),

    % Constrain Durations list to always correlate with Sequence
    constrainDurations(Durations, Sequence, FixedDurations),

    % Constrain Musicians list to always correlate with Sequence
    musicians(FixedMusicians),
    constrainMusicians(Musicians, Sequence, FixedMusicians),

    computeWaitTime(Musicians, Durations, WaitTimes),
    WaitTime #:: 0..1.0Inf,
    WaitTime #= sum(WaitTimes),

    bb_min(search(Sequence, 0, first_fail, indomain, complete, []),
    WaitTime, bb_options{strategy:continue, timeout:TimeOut}).

constrainDurations(Durations, Sequence, FixedDurations):-
    length(FixedDurations, Pieces),
    length(Durations, Pieces),
    Durations #:: FixedDurations,
    correlate(Sequence, Durations, FixedDurations).

constrainMusicians(Musicians, Sequence, FixedMusicians):-
    length(FixedMusicians, N),
    length(Musicians, N),
    constrainMusician(Musicians, Sequence, FixedMusicians).

constrainMusician([], _, []).
constrainMusician([M | Ms], Sequence, [F | Fs]):-
    length(F, Pieces),
    length(M, Pieces),
    M #:: [0, 1],
    correlate(Sequence, M, F),
    constrainMusician(Ms, Sequence, Fs).

correlate([], [], _).
correlate([S | Ss], [D | Ds], ValuesList):-
    element(S, ValuesList, D),
    correlate(Ss, Ds, ValuesList).


computeWaitTime([], _, []).
computeWaitTime([Musician | Musicians], Durations, [WaitTime | Rest]):-
    % Explanation : Lets say we have 9 pieces and this musician plays on 3, 4, 6, 8
    % OneIndexList  : 0 0 3 4 0 6 0 8 0
    % ZeroIndexList : 1 2 0 0 5 0 7 0 9
    % As we can see from OneIndexList the musician will have to stay between index 3-8
    % That means that MinIndex will have to be 3 and MaxIndex will have to be 8
    % We cannot use min(OneIndexList) to find MinIndex because it will return 0. So we need to modify the list.
    % We know that the highest possible number in OneIndexList is Pieces, so
    % in order to remove all zeros from OneIndexList, we can add Pieces + 1 to all zeros
    % Then we can find minimum non-zero number in OneIndexList
    % To find MaxIndex we can use max(OneIndexList)
    % Now, in order to find during which pieces the musician will wait we can use the opposite list of OneIndexList, ZeroIndexList
    % In that list we will find all number that is NOT 0 and is between MinIndex and MaxIndex in list.
    % We will get a list that looks like this : 0 0 0 0 5 0 7 0 0
    % Now using this list we will find which durations to add to WaitTime and move on to next musician

    length(Musician, Pieces),
    length(OneIndexList, Pieces),
    % OneIndexList is a list that contains elements with domain [0, IndexInList] aka [0, 1], [0, 2] ...
    % We will use that list to find all pieces during which the musician will play.
    limitIndexList(OneIndexList, 1),
    findTrueIndices(Musician, OneIndexList),

    
    length(NonZeroOneIndexList, Pieces),
    MaxNumber is Pieces + 1,
    NonZeroOneIndexList #:: 1..MaxNumber,
    nonZerify(OneIndexList, MaxNumber, NonZeroOneIndexList),

    % MinIndex and MaxIndex are the first and last piece during which the musician will play
    MinIndex #= min(NonZeroOneIndexList),
    MaxIndex #= max(OneIndexList),

    % ZeroIndexList is the exact opposite of OneIndexList
    length(ZeroIndexList, Pieces),
    limitIndexList(ZeroIndexList, 1),
    findFalseIndices(Musician, ZeroIndexList),

    % So WaitTime for this musician is all elements of ZeroIndexList between MinIndex and MaxIndex that are NOT 0

    % Make a list of [0, 1] where 1 shows the indices the musician is waiting
    % We use those indices to add WaitTime from Durations.
    length(WaitList, Pieces),
    WaitList #:: [0, 1],
    findWaitList(MinIndex, MaxIndex, ZeroIndexList, WaitList, 1),
    findWaitTimes(WaitList, WaitTimes, Durations),
    WaitTime #= sum(WaitTimes),

    computeWaitTime(Musicians, Durations, Rest).

findWaitList(_Min, _Max, [], [], _N).
findWaitList(Min, Max, [I | Is], [Bool | Bools], N):-
    N1 is N + 1,
    Bool #= (N #>= Min and N #=< Max and I #\= 0),
    findWaitList(Min, Max, Is, Bools, N1).

findWaitTimes([], [], []).
findWaitTimes([I | Is], [W | Ws], [D | Ds]):-
    (I #= 0) => (W #= 0),
    (I #= 1) => (W #= D),
    findWaitTimes(Is, Ws, Ds).

% Wherever you find a 0 add ValueToAdd to it
nonZerify([], _, []).
nonZerify([I | Is], ValueToAdd, [Ni | Nis]):-
    (I #=  0) => (Ni #= I + ValueToAdd),
    (I #\= 0) => (Ni #= I),
    nonZerify(Is, ValueToAdd, Nis).


% Foreach musician find during which pieces he is playing (All 1s)
findTrueIndices([], []).
findTrueIndices([Piece | Pieces], [I | Is]):-
    (Piece #= 0 and I #= 0) or (Piece #\= 0  and I #\= 0),
    findTrueIndices(Pieces, Is).

% Foreach musician find during which pieces he is not playing (All 0s)
findFalseIndices([], []).
findFalseIndices([Piece | Pieces], [I | Is]):-
    (Piece #\= 0 and I #= 0) or (Piece #= 0  and I #\= 0),
    findFalseIndices(Pieces, Is).

limitIndexList([], _).
limitIndexList([I | Is], N):-
    I #:: [0, N],
    N1 is N + 1,
    limitIndexList(Is, N1).
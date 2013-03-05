:- [war_of_life].
    
test_strategy(N, Strat1, Strat2):-
    test_strat_helper(N, Strat1, Strat2, 0, 0, 0, 0, 250, 0).
    
test_strat_helper(0, _, _, B, R, D, Max, Min, Total):-
    format('~nblue score = ~w~nredscore = ~w~ndraws = ~w~nMax = ~w~nMin = ~w~nTotal = ~w~n', [B,R,D,Max,Min,Total]).

test_strat_helper(N, S1, S2, B, R, D, Max, Min, Total):-
    NewN is N - 1,
    play(quiet, S1, S2, Moves, Winner),
    biggest(Max, Moves, Big),
    smallest(Min, Moves, Small),
    NewTotal is Total + Moves,
    ((Winner = 'r'
      ->NewR is R + 1,
      %%format('~nwinner = ~w~nN =~w~n', [Winner, N]),
      test_strat_helper(NewN, S1, S2, B, NewR, D, Big, Small, NewTotal); 
      Winner = 'b' 
      ->NewB is B + 1,
      %%format('~nwinner = ~w~nN =~w~n', [Winner, N]),
      test_strat_helper(NewN, S1, S2, NewB, R, D, Big, Small, NewTotal));
      NewD is D + 1,
      %%format('~nwinner = ~w~nN =~w~n', [Winner, N]),
      test_strat_helper(NewN, S1, S2, B, R, NewD, Big, Small, NewTotal)).


biggest(A,B,A):-
  A > B.
biggest(A,B,B):-
  A =< B.

smallest(A,B,A):-
  A < B.
smallest(A,B,B):-
  A >= B.

get_time(T):-
    statistics(runtime,[Start|_]),
    test_strategy(1000, random, random),
    statistics(runtime,[Finish|_]),
    T is Start - Finish.

/*
find_alive('r', [_, AliveReds], AliveReds).
find_alive('b', [AliveBlues, _], AliveBlues).

next_gen_for_player('r', [_, NewReds], NewReds).
next_gen_for_player('b', [NewBlues, _], NewBlues).

get_pos_moves(Player, [AliveBlues,AliveReds], PossMoves):-
    findall([A, B, MA, MB],
           (member([A, B], Alive),
            find_alive(Player, [AliveBlues, AliveReds], Alive),
            neighbour_position(A,B,[MA,MB]),
            \+ member([MA, MB], AliveReds),
	          \+ member([MA, MB], AliveBlues)), 
            PossMoves).


getOpponent('r','b').
getOpponent('b','r').

next_gen('r', NextAlivePlayer, [AliveBlues, _], [NewBlues, NewReds]):-
    next_generation([NextAlivePlayer, AliveBlues], [NewBlues, NewReds]).  
next_gen('b', NextAlivePlayer, [_, AliveReds], [NewBlues, NewReds]):-
    next_generation([NextAlivePlayer, AliveReds], [NewBlues, NewReds]). 

has_minimum(Player, Move, PossMoves, [AliveBlues, AliveReds], [NewBlues, NewReds]):-
    member(Move, PossMoves),
    find_alive(Player, [AliveBlues, AliveReds], Alive),
    alter_board(Move, Alive, NextAlivePlayer),
    next_gen(Player, NextAlivePlayer, [AliveBlues, AliveReds], [NewBlues, NewReds]),
    getOpponent(Player, Opponent),
    next_gen_for_player(Opponent, [NewBlues, NewReds], NextGen),
     length(NextGen, Min),
     \+ (member(Others, PossMoves), 
         alter_board(Others, Alive, NextAlivePlayer1),
         next_gen(Player, NextAlivePlayer1, [AliveBlues, AliveReds], [NewBlues1, NewReds1]),
         next_gen_for_player(Opponent, [NewBlues1, NewReds1], NextGen1),
         length(NextGen1, Min1),
         Min1 < Min).
*/     
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% BLOODLUST STRATEGY
bloodlust('r', [AliveBlues,AliveReds],  NewBoard, Move):-
    findall([A, B, MA, MB],
           (member([A, B], AliveReds),
            neighbour_position(A,B,[MA,MB]),
            \+ member([MA, MB], AliveReds),
	          \+ member([MA, MB], AliveBlues)), 
            PossMoves),
    bloodlust_move('r',PossMoves,[AliveBlues,AliveReds],NewBoard,Move).
bloodlust('b', [AliveBlues,AliveReds],  NewBoard, Move):-
    findall([A, B, MA, MB],
           (member([A, B], AliveBlues),
            neighbour_position(A,B,[MA,MB]),
            \+ member([MA, MB], AliveReds),
	          \+ member([MA, MB], AliveBlues)), 
            PossMoves),
    bloodlust_move('b',PossMoves,[AliveBlues,AliveReds],NewBoard,Move).

bloodlust_move('r', PossMoves, [AliveBlues, AliveReds], [AliveBlues, NextReds], Move) :-
    member(Move, PossMoves),
    alter_board(Move, AliveReds, NextReds),
    next_generation([AliveBlues, NextReds], [NewBlues, _]),
    length(NewBlues, Least),
    \+ (member(Others, PossMoves), 
        alter_board(Others, AliveReds, NextReds1),
        next_generation([AliveBlues, NextReds1], [NewBlues1, _]),
        length(NewBlues1, L),
        L < Least). 
bloodlust_move('b', PossMoves, [AliveBlues, AliveReds], [NextBlues, AliveReds], Move) :-
    member(Move, PossMoves),
    alter_board(Move, AliveBlues, NextBlues),
    next_generation([NextBlues, AliveReds], [_, NewReds]),
    length(NewReds, Least),
    \+ (member(Others, PossMoves), 
        alter_board(Others, AliveBlues, NextBlues1),
        next_generation([NextBlues1, AliveReds], [_, NewReds1]),
        length(NewReds1, L),
        L < Least).  

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% SELF_PRESERVATION STRATEGY
self_preservation('r', [AliveBlues,AliveReds],  NewBoard, Move):-
    findall([A, B, MA, MB],
           (member([A, B], AliveReds),
            neighbour_position(A,B,[MA,MB]),
            \+ member([MA, MB], AliveReds),
	          \+ member([MA, MB], AliveBlues)), 
            PossMoves),
    self_preservation_move('r',PossMoves,[AliveBlues,AliveReds],NewBoard,Move).
self_preservation('b', [AliveBlues,AliveReds],  NewBoard, Move):-
    findall([A, B, MA, MB],
           (member([A, B], AliveBlues),
            neighbour_position(A,B,[MA,MB]),
            \+ member([MA, MB], AliveReds),
	          \+ member([MA, MB], AliveBlues)), 
            PossMoves),
    self_preservation_move('b',PossMoves,[AliveBlues,AliveReds],NewBoard,Move).

self_preservation_move('r', PossMoves, [AliveBlues, AliveReds], [AliveBlues, NextReds], Move) :-
    member(Move, PossMoves),
    alter_board(Move, AliveReds, NextReds),
    next_generation([AliveBlues, NextReds], [_, NewReds]),
    length(NewReds, Most),
    \+ (member(Others, PossMoves), 
        alter_board(Others, AliveReds, NextReds1),
        next_generation([AliveBlues, NextReds1], [_, NewReds1]),
        length(NewReds1, M),
        M > Most). 
self_preservation_move('b', PossMoves, [AliveBlues, AliveReds], [NextBlues, AliveReds], Move) :-
    member(Move, PossMoves),
    alter_board(Move, AliveBlues, NextBlues),
    next_generation([NextBlues, AliveReds], [NewBlues, _]),
    length(NewBlues, Most),
    \+ (member(Others, PossMoves), 
        alter_board(Others, AliveBlues, NextBlues1),
        next_generation([NextBlues1, AliveReds], [NewBlues1, _]),
        length(NewBlues1, M),
        M > Most).  

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% LAND_GRAB STRATEGY
land_grab('r', [AliveBlues,AliveReds],  NewBoard, Move):-
    findall([A, B, MA, MB],
           (member([A, B], AliveReds),
            neighbour_position(A,B,[MA,MB]),
            \+ member([MA, MB], AliveReds),
	          \+ member([MA, MB], AliveBlues)), 
            PossMoves),
    land_grab_move('r',PossMoves,[AliveBlues,AliveReds],NewBoard,Move).
land_grab('b', [AliveBlues,AliveReds],  NewBoard, Move):-
    findall([A, B, MA, MB],
           (member([A, B], AliveBlues),
            neighbour_position(A,B,[MA,MB]),
            \+ member([MA, MB], AliveReds),
	          \+ member([MA, MB], AliveBlues)), 
            PossMoves),
    land_grab_move('b',PossMoves,[AliveBlues,AliveReds],NewBoard,Move).
land_grab_move('r', PossMoves, [AliveBlues, AliveReds], [AliveBlues, NextReds], Move) :-
    member(Move, PossMoves),
    alter_board(Move, AliveReds, NextReds),
    next_generation([AliveBlues, NextReds], [NewBlues, NewReds]),
    length(NewReds, R),
    length(NewBlues, B),
    Diff is R - B,
    \+ (member(Others, PossMoves), 
        alter_board(Others, AliveReds, NextReds1),
        next_generation([AliveBlues, NextReds1], [NewBlues1, NewReds1]),
        length(NewReds1, R1),
        length(NewBlues1, B1),
        Diff1 is R1 - B1, 
        Diff1 > Diff). 
land_grab_move('b', PossMoves, [AliveBlues, AliveReds], [NextBlues, AliveReds], Move) :-
    member(Move, PossMoves),
    alter_board(Move, AliveBlues, NextBlues),
    next_generation([NextBlues, AliveReds], [NewBlues, NewReds]),
    length(NewBlues, B),
    length(NewReds, R),
    Diff is B - R,
    \+ (member(Others, PossMoves), 
        alter_board(Others, AliveBlues, NextBlues1),
        next_generation([NextBlues1, AliveReds], [NewBlues1, NewReds1]),
        length(NewBlues1, B1),
        length(NewReds1, R1),
        Diff1 is B1 - R1,
        Diff1 > Diff).  

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% LAND_GRAB STRATEGY
/*
minimax(Player, [AliveBlues,AliveReds],  NewBoard, Move):-
    get_pos_moves(Player, [AliveBlues, AliveReds], PossMoves),
    minimax_move(Player, PossMoves, [AliveBlues,AliveReds], NewBoard, Move).

minimax_move('r', PossMoves, [AliveBlues, AliveReds], [NewBlues, NewReds], Move) :-
    member(Move, PossMoves),
    alter_board(Move, AliveReds, NextReds),
    next_generation([AliveBlues, NextReds], [NewBlues, NewReds]),
    get_pos_moves('b', [NewBlues, NewReds], NextPossMoves),
    min('b', NextPossMoves, [NewBlues, NewReds], Move, MinScore),
    \+ ( member(Others, PossMoves),
    alter_board(Others, AliveReds, NextReds1),
    next_generation([AliveBlues, NextReds1], [NewBlues1, NewReds1]),
    get_pos_moves('b', [NewBlues1, NewReds1], NextPossMoves1),
    min('b', NextPossMoves1, [NewBlues1, NewReds1], _, MinScore1),
    MinScore1 > MinScore).
minimax_move('b', PossMoves, [AliveBlues, AliveReds], [NewBlues, NewReds], Move) :-
    member(Move, PossMoves),
    alter_board(Move, AliveBlues, NextBlues),
    next_generation([NextBlues, AliveReds], [NewBlues, NewReds]),
    get_pos_moves('r', [NewBlues, NewReds], NextPossMoves),
    min('r', NextPossMoves, [NewBlues, NewReds], Move, MinScore),
    \+ ( member(Others, PossMoves),
    alter_board(Others, AliveBlues, NextBlues1),
    next_generation([NextBlues1, AliveReds], [NewBlues1, NewReds1]),
    get_pos_moves('r', [NewBlues1, NewReds1], NextPossMoves1),
    min('r', NextPossMoves1, [NewBlues1, NewReds1], _, MinScore1),
    MinScore1 > MinScore).

min('r', PossMoves, [AliveBlues, AliveReds], Move, Diff):-
    member(Move, PossMoves),
    alter_board(Move, AliveReds, NextReds),
    next_generation([AliveBlues, NextReds], [NewBlues, NewReds]),
    length(NewReds, R),
    length(NewBlues, B),
    Diff is R - B,
    \+ (member(Others, PossMoves),
        alter_board(Others, AliveReds, NextReds1),
        next_generation([AliveBlues, NextReds1], [NewBlues1, NewReds1]),
        length(NewBlues1, B1),
        length(NewReds1, R1),
        Diff1 is R1 - B1,
        Diff1 < Diff).      
min('b', PossMoves, [AliveBlues, AliveReds], Move, Diff):-
    member(Move, PossMoves),
    alter_board(Move, AliveBlues, NextBlues),
    next_generation([NextBlues, AliveReds], [NewBlues, NewReds]),
    length(NewReds, R),
    length(NewBlues, B),
    Diff is R - B,
    \+ (member(Others, PossMoves),
        alter_board(Others, AliveBlues, NextBlues1),
        next_generation([NextBlues1, AliveReds], [NewBlues1, NewReds1]),
        length(NewBlues1, B1),
        length(NewReds1, R1),
        Diff1 is R1 - B1,
        Diff1 < Diff). 

land_grab('r', [AliveBlues,AliveReds],  NewBoard, Move):-
*/

minimax('r', [AliveBlues, AliveReds], NewBoard, Move):-
  findall([A, B, MA, MB],
           (member([A, B], AliveReds),
            neighbour_position(A,B,[MA,MB]),
            \+ member([MA, MB], AliveReds),
	          \+ member([MA, MB], AliveBlues)), 
            PossMoves),
    minimax_move('r',PossMoves,[AliveBlues,AliveReds],NewBoard,Move).  
minimax('b', [AliveBlues, AliveReds], NewBoard, Move):-
  findall([A, B, MA, MB],
           (member([A, B], AliveReds),
            neighbour_position(A,B,[MA,MB]),
            \+ member([MA, MB], AliveReds),
	          \+ member([MA, MB], AliveBlues)), 
            PossMoves),
    minimax_move('r',PossMoves,[AliveBlues,AliveReds],NewBoard,Move).



minimax_move('b', PossMoves, [AliveBlues, AliveReds], NewBoard, Move):-
  member(Move,PossMoves),  
  double_look('b', Move, [AliveBlues, AliveReds], WeightRoot, WeightLower,NewBoard),
  TotalWeight is WeightRoot + WeightLower,
  \+ ( member(MoveOther,PossMoves),  
  double_look('b', MoveOther, [AliveBlues, AliveReds], WeightRootOther, WeightLowerOther,NewBoard),
  TotalWeightOther is WeightRootOther + WeightLowerOther, 
  TotalWeightOther > TotalWeight).


minimax_move('r', PossMoves, [AliveBlues, AliveReds], NewBoard, Move):-
  member(Move,PossMoves),  
  double_look('r', Move, [AliveBlues, AliveReds], WeightRoot, WeightLower,NewBoard),
  TotalWeight is WeightRoot + WeightLower,
  \+ ( member(MoveOther,PossMoves),  
  double_look('r', MoveOther, [AliveBlues, AliveReds], WeightRootOther, WeightLowerOther,NewBoard),
  TotalWeightOther is WeightRootOther + WeightLowerOther, 
  TotalWeightOther > TotalWeight).

double_look('b',Move,[AliveBlues,AliveReds],Weight,WeightLower,[ChangedBlues,AliveReds]):-
  alter_board(Move, AliveBlues, ChangedBlues),
  next_generation([ChangedBlues, AliveReds],[NewBlues, NewReds]),
  length(NewBlues, WeightBlue),
  length(NewReds, WeightRed),
  Weight is WeightBlue - WeightRed,
  
  findall([A, B, MA, MB],
           (member([A, B], NewReds),
            neighbour_position(A,B,[MA,MB]),
            \+ member([MA, MB], NewReds),
	          \+ member([MA, MB], NewBlues)), 
            OppPossMoves),
  member(OppMove, OppPossMoves),
  alter_board(OppMove, NewReds, ChangedReds),
  next_generation([NewBlues, ChangedReds],[NewNewBlues, NewNewReds]),
  length(NewNewBlues, NewWeightBlue),
  length(NewNewReds, NewWeightRed),
  WeightLower is NewWeightBlue - NewWeightRed,
  \+ (member(OtherOppMove, OppPossMoves),
    alter_board(OtherOppMove, NewReds, ChangedRedsOther),
    next_generation([NewBlues, ChangedRedsOther],[NewNewBluesOther, NewNewRedsOther]),
    length(NewNewBluesOther, NewWeightBlue1),
    length(NewNewRedsOther, NewWeightRed1),
    Weight2 is NewWeightBlue1 - NewWeightRed1,
    Weight2 < WeightLower).
  
     
double_look('r',Move,[AliveBlues,AliveReds],Weight,WeightLower,[AliveBlues,ChangedReds]):-
  alter_board(Move, AliveReds, ChangedReds),
  next_generation([AliveBlues, ChangedReds],[NewBlues, NewReds]),
  length(NewBlues, WeightBlue),
  length(NewReds, WeightRed),
  Weight is WeightRed - WeightBlue,
  
  findall([A, B, MA, MB],
           (member([A, B], NewBlues),
            neighbour_position(A,B,[MA,MB]),
            \+ member([MA, MB], NewReds),
	          \+ member([MA, MB], NewBlues)), 
            OppPossMoves),
  member(OppMove, OppPossMoves),
  alter_board(OppMove, NewBlues, ChangedBlues),
  next_generation([ChangedBlues, NewReds],[NewNewBlues, NewNewReds]),
  length(NewNewBlues, NewWeightBlue),
  length(NewNewReds, NewWeightRed),
  WeightLower is NewWeightRed - NewWeightBlue,
  \+ (member(OtherOppMove, OppPossMoves),
    alter_board(OtherOppMove, NewBlues, ChangedBluesOther),
    next_generation([ChangedBluesOther, NewReds],[NewNewBluesOther, NewNewRedsOther]),
    length(NewNewBluesOther, NewWeightBlue1),
    length(NewNewRedsOther, NewWeightRed1),
    Weight2 is NewWeightRed1 - NewWeightBlue1,
    Weight2 < WeightLower).


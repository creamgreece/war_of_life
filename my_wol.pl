test_strategy(N, Strat1, Strat2):-
    test_strat_helper(N, Strat1, Strat2, 0, 0, 0, 0, 250,0).
    
test_strat_helper(0, _, _, B, R, D, Max, Min,Total):-
  format('~nblue score = ~w~nredscore = ~w~ndraws = ~w~nMax = ~w~nMin = ~w~nTotal = ~w~n', [B,R,D,Max,Min,Total]).

test_strat_helper(N, S1, S2, B, R, D, Max,Min,Total):-
  NewN is N - 1,
  play(quiet, S1, S2, Moves, Winner),
  biggest(Max,Moves,Big),
  smallest(Min,Moves,Small),
  NewTotal is Total + Moves,
  ((Winner = 'r'
  ->NewR is R + 1,
  %%format('~nwinner = ~w~nN =~w~n', [Winner,N]),
    test_strat_helper(NewN, S1, S2, B, NewR, D,Big,Small,NewTotal)
  ; Winner = 'b' 
  ->NewB is B + 1,
  %%format('~nwinner = ~w~nN =~w~n', [Winner,N]),
    test_strat_helper(NewN, S1, S2, NewB, R, D,Big,Small,NewTotal))
  ;NewD is D + 1,
  %%format('~nwinner = ~w~nN =~w~n', [Winner,N]),
    test_strat_helper(NewN, S1, S2, B, R, NewD,Big,Small,NewTotal)).


biggest(A,B,A):-
  A > B.
biggest(A,B,B):-
  A =< B.

smallest(A,B,A):-
  A < B.
smallest(A,B,B):-
  A >= B.

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% BLOODLUST STRATEGY
bloodlust('r', [AliveBlues, AliveReds],  [NewAliveBlues, NewAliveReds], Move):-
 findall([A,B,MA,MB],(member([A,B], [AliveBlues,AliveReds]),
                      neighbour_position(A,B,[MA,MB]),
                      \+member([MA,MB],AliveReds),
	                    \+member([MA,MB],AliveBlues)),PossMoves),
     setof((NewAliveBlues,[MA,MB]),alter_board([MA,MB],AliveBlues,NewAliveBlues),[(NewAliveBlues,Move)|_]).
bloodlust('b', [AliveBlues, AliveReds],  [NewAliveBlues, NewAliveReds], Move):-
 findall([A,B,MA,MB],(member([A,B], [AliveBlues,AliveReds]),
                      neighbour_position(A,B,[MA,MB]),
                      \+member([MA,MB],AliveReds),
	                    \+member([MA,MB],AliveBlues)),PossMoves),
     setof((NewAliveReds,[MA,MB]),alter_board([MA,MB],AliveReds,NewAliveReds),[(NewAliveReds,Move)|_]).
/*

 bloodiest_move(0,PossMoves,SmallestRed,Move) 
 alterBoard(Move, AliveReds, NewAliveReds),
 smallest(LeastReds,AliveReds, LeastReds),
  Move
  Least Reds

random_move(Alive, OtherPlayerAlive, Move) :-
 findall([A,B,MA,MB],(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
	              \+member([MA,MB],Alive),
	              \+member([MA,MB],OtherPlayerAlive)),
	 PossMoves),
 length(PossMoves,L),
 LP1 is L + 1,
 random(1, LP1, Pos),
 nth1(Pos, PossMoves, Move).

move_piece('b', random, [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :- 
 random_move(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

move_piece('r', random, [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
 random_move(AliveReds, AliveBlues, Move),
 alter_board(Move, AliveReds, NewAliveReds).






move_piece('r', bloodlust, [AliveBlues, AliveReds], [NewAliveBlues, NewAliveReds], Move) :-
 bloodlust('r', [AliveBlues, AliveReds],  [NewAliveBlues, NewAliveReds], Move),
 alter_board(Move, AliveReds, NewAliveReds).























self_preservation(PieceColour, Board, NewBoard, Move):-
land_grab(PieceColour, Board, NewBoard, Move):-
minimax(PieceColour, Board, NewBoard, Move):-*/

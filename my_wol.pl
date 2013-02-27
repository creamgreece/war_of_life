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














bloodiest_move([[4,4,3,4],[4,4,3,5],[4,4,4,3],[4,4,4,5],[4,4,5,3],[4,4,5,4],[4,4,5,5],[7,8,6,7],[7,8,6,8],[7,7,6,6],[7,7,6,7],[7,7,6,8],[7,7,7,6],[7,7,8,6],[8,7,7,6],[8,7,8,6]], [[[2,2],[2,3],[2,4],[3,3]],[[4,4],[8,8],[7,8],[7,7],[8,7]]],Move, 65).



bloodiest_move([],_,Move,Least).
bloodiest_move([[A,B,MA,MB]|PossMoves],[AliveBlues,AliveReds],[A,B,MA,MB],Least) :-
   alter_board([A,B,MA,MB],AliveReds,NextReds),
   next_generation([AliveBlues,NextReds],[NextBlues,_]),
    length(NextBlues,L),
    Least > L,
    bloodiest_move(PossMoves,[AliveBlues,AliveReds],[A,B,MA,MB],L).
bloodiest_move([[A,B,MA,MB]|PossMoves],[AliveBlues,AliveReds],Move,Least) :-
   alter_board([A,B,MA,MB],AliveReds,NextReds),
   next_generation([AliveBlues,NextReds],[NextBlues,_]),
    length(NextBlues,L),
    Least < L,
    bloodiest_move(PossMoves,[AliveBlues,AliveReds],Move,Least).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% BLOODLUST STRATEGY
bloodlust('r', [AliveBlues, AliveReds],  [NewAliveBlues, NewAliveReds], Move):-
 findall([A,B,MA,MB],(member([A,B], AliveReds),
                      neighbour_position(A,B,[MA,MB]),
                      \+member([MA,MB],AliveReds),
	                    \+member([MA,MB],AliveBlues)),PossMoves),
     setof((NewAliveBlues,[MA,MB]),alter_board([MA,MB],AliveBlues,NewAliveBlues),[(NewAliveBlues,Move)|_]).
bloodlust('b', [AliveBlues, AliveReds],  [NewAliveBlues, NewAliveReds], Move):-
 findall([A,B,MA,MB],(member([A,B], AliveBlues),
                      neighbour_position(A,B,[MA,MB]),
                      \+member([MA,MB],AliveReds),
	                    \+member([MA,MB],AliveBlues)),PossMoves),
     setof((NewAliveReds,[MA,MB]),alter_board([MA,MB],AliveReds,NewAliveReds),[(NewAliveReds,Move)|_]).
/*    
   

testConfig([[2,2],[2,3],[2,4],[3,3]],[[4,4],[8,8],[7,8],[7,7],[8,7]]).

test(Board):-
testConfig(Board),
drawBoard(Board),


findall([A,B,MA,MB],(member([A,B], [[4,4],[8,8],[7,8],[7,7],[8,7]]),neighbour_position(A,B,[MA,MB]),\+member([MA,MB],[[4,4],[8,8],[7,8],[7,7],[8,7]]),\+member([MA,MB],[[2,2],[2,3],[2,4],[3,3]])),PossMoves),bloodiest_move(PossMoves,[[[2,2],[2,3],[2,4],[3,3]],[[4,4],[8,8],[7,8],[7,7],[8,7]]],Move,65). 



findall((L,[MA,MB]),(alter_board([A,B,MA,MB],[[4,4],[8,8],[7,8],[7,7],[8,7]],NewReds), next_generation([[[2,2],[2,3],[2,4],[3,3]],NewReds],[NextBlues,_]), length(NewBlues,L), member([A,B,MA,MB],PossMoves)), Ans). 



[[4,4,3,4],[4,4,3,5],[4,4,4,3],[4,4,4,5],[4,4,5,3],[4,4,5,4],[4,4,5,5],[7,8,6,7],[7,8,6,8],[7,7,6,6],[7,7,6,7],[7,7,6,8],[7,7,7,6],[7,7,8,6],[8,7,7,6],[8,7,8,6]]


next_generation([[[2,2],[2,3],[2,4],[3,3]],NewReds],NewBoard).

alter_board([A,B,MA,MB], Alives, NewAlives) :-

 alterBoard(Move, AliveReds, NewAliveReds),
 smallest(LeastReds,AliveReds, LeastReds),
  Move
  Least Reds


alter_board([4,4,4,3],[[1,8],[4,4],[8,8]],NewReds), next_generation([[[2,2],[2,3],[2,4],[3,3]],NewReds],NewBoard),write(NewBoard).


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

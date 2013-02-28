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

bloodiest_move('r', PossMoves, [AliveBlues, AliveReds], [NewBlues, NewReds], Move) :-
    member(Move, PossMoves),
    alter_board(Move, AliveReds, NextReds),
    next_generation([AliveBlues, NextReds], [NewBlues, NewReds]),
    length(NewBlues, Least),
    \+ (member(Others, PossMoves), 
        alter_board(Others, AliveReds, NextReds1),
        next_generation([AliveBlues, NextReds1], [NewBlues1, _]),
        length(NewBlues1, L),
        L < Least). 
bloodiest_move('b', PossMoves, [AliveBlues, AliveReds], [NewBlues, NewReds], Move) :-
    member(Move, PossMoves),
    alter_board(Move, AliveBlues, NextBlues),
    next_generation([NextBlues, AliveReds], [NewBlues, NewReds]),
    length(NewReds, Least),
    \+ (member(Others, PossMoves), 
        alter_board(Others, AliveBlues, NextBlues1),
        next_generation([NextBlues1, AliveReds], [_, NewReds1]),
        length(NewReds1, L),
        L < Least).     


%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% BLOODLUST STRATEGY
bloodlust(Player, [AliveBlues,AliveReds],  NewBoard, Move):-
    findall([A, B, MA, MB],
           (member([A, B], AliveReds),
            neighbour_position(A,B,[MA,MB]),
            \+ member([MA, MB], AliveReds),
	          \+ member([MA, MB], AliveBlues)), 
            PossMoves),
    bloodiest_move(Player,PossMoves,[AliveBlues,AliveReds],NewBoard,Move).


                      
/*    
   

testConfig([[2,2],[2,3],[2,4],[3,3]],[[4,4],[8,8],[7,8],[7,7],[8,7]]).

test(Board):-
testConfig(Board),
drawBoard(Board),


findall([A,B,MA,MB],(member([A,B], [[4,4],[8,8],[7,8],[7,7],[8,7]]),neighbour_position(A,B,[MA,MB]),\+member([MA,MB],[[4,4],[8,8],[7,8],[7,7],[8,7]]),\+member([MA,MB],[[2,2],[2,3],[2,4],[3,3]])),PossMoves),bloodiest_move(PossMoves,[[[2,2],[2,3],[2,4],[3,3]],[[4,4],[8,8],[7,8],[7,7],[8,7]]],Move,65). 



findall((L,[MA,MB]),(alter_board([A,B,MA,MB],[[4,4],[8,8],[7,8],[7,7],[8,7]],NewReds), next_generation([[[2,2],[2,3],[2,4],[3,3]],NewReds],[NextBlues,_]), length(NewBlues,L), member([A,B,MA,MB],PossMoves)), Ans). 



[[4,4,3,4],[4,4,3,5],[4,4,4,3],[4,4,4,5],[4,4,5,3],[4,4,5,4],[4,4,5,5],[7,8,6,7],[7,8,6,8],[7,7,6,6],[7,7,6,7],[7,7,6,8],[7,7,7,6],[7,7,8,6],[8,7,7,6],[8,7,8,6]]


next_generation([[[2,2],[2,3],[2,4],[3,3]],NewReds],NewBoard).


alter_board([4,4,4,3],[[1,8],[4,4],[8,8]],NewReds), next_generation([[[2,2],[2,3],[2,4],[3,3]],NewReds],NewBoard),write(NewBoard).



self_preservation(PieceColour, Board, NewBoard, Move):-
land_grab(PieceColour, Board, NewBoard, Move):-
minimax(PieceColour, Board, NewBoard, Move):-*/

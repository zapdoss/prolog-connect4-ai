%%%%%%%%%%%%%   AI Rules   %%%%%%%%%%%%%%

printAIlist([]).
printAIlist([H|T]):-
	getResult(H,S,_),
	(
	    abs(S) < 1000000000,
	    format('  (~d)  ',[S]);
	    S =< -1000000000,
	    depth(D),
	    S1 is S/1000000000,
	    format('  (~d L)  ',[D+S1]);
	    S >= 1000000000 ,
	    depth(D),
	    S1 is S/1000000000,
	    format('  (~d W)  ',[D-S1])
	),
	printAIlist(T).

getResult([H,T|_],H,T).

doBest(Color, Depth, BestScore, BestMove):-
	size(C,_),
	setof(X,between(1,C,X),MoveList),
	minimax(MoveList,Depth, Color, Result),
	sort(Result,SortedResult),
	(depth(D),Depth == D,printAIlist(Result),writeln('');true),
	(   Color == r ->
			  nth0(0,SortedResult,Move),
			  getResult(Move,BestScore,BestMove);
	    Color == y ->
	                  length(SortedResult,Len),
			  nth1(Len,SortedResult,Move),
	                  getResult(Move,BestScore,BestMove)
	).

minimax([],_,_,[]).
minimax([H|T],Depth, Color, L):-
	Color == r,
	minimax(T,Depth,Color,L1),
	top(H,Height),
	size(_,R),
	(
	    Height == R -> L = L1,!;

	    insert(H,Color),
	    top(H,Hight),
	    checkStatus(H,Hight,Color),
	    (    win(r) ->
	            V0 is -1000000000  * (Depth + 1),
		    append([[V0,H]],L1,L);
		 win(y)	->
		    V0 is 1000000000   * (Depth + 1),
		    append([[V0,H]],L1,L);
                 draw() ->
		    append([[0,H]],L1,L);
	         not(win(r);win(y);draw())  ->
	          (
		    Depth == 0 ->
		        score(r,Vr),score(y,Vy),
		        V0 is Vy - Vr,
		        append([[V0,H]],L1,L);

		    Depth > 0   ->
		       NewDep is Depth - 1,
		       doBest(y, NewDep, BestScore,_),
		       append([[BestScore,H]],L1,L)
		 )
	    ),
	    resetStatus(),
	    remove(H)
	).



minimax([H|T],Depth, Color, L):-
	Color == y,
	minimax(T,Depth,Color,L1),
	top(H,Height),
	size(_,R),
	(
	    Height == R -> L = L1,!;

	    insert(H,Color),
	    top(H,Hight),
	    checkStatus(H,Hight,Color),
	    (    win(r) ->
	            V0 is -1000000000  * (Depth + 1),
		    append([[V0,H]],L1,L);
		 win(y) ->
	            V0 is 1000000000   * (Depth + 1),
		    append([[V0,H]],L1,L);
		 draw() ->
	            append([[0,H]],L1,L);

	        not(win(r);win(y);draw())  ->
	         (  Depth == 0 ->
		     score(r,Vr),score(y,Vy),
		     V0 is Vy - Vr,
		     append([[V0,H]],L1,L);

		   Depth > 0   ->
		    NewDep is Depth - 1,
		    doBest(r, NewDep, BestScore,_),
		    append([[BestScore,H]],L1,L)
		 )
	    ),
	    resetStatus(),
	    remove(H)
	).

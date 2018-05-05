checkStatus(X,Y,Color):-
	check(X,Y),
	(   Color == r -> assert(win(r)),assert(lose(y));
	    Color == y -> assert(win(y)),assert(lose(r))
	).
checkStatus(_,_,_).

draw():-
	size(C,R),
	aggregate_all(count,top(_,R),Cnt),
	Cnt == C.

traverse(C,R,IncC,IncR,Res):-
       NewC is C + IncC,
       NewR is R + IncR,
       piece(C,R,C1),
       piece(NewC,NewR,C2),
       C1 == C2,
       traverse(NewC,NewR,IncC,IncR,Res1),
       Res is Res1 + 1,!.
traverse(_,_,_,_,Res):-
	Res is 1.

%Horizontal Check
check(X,Y):-
	traverse(X,Y,1,0,R1),
	traverse(X,Y,-1,0,R2),
	R is R1 + R2 - 1  ,
	R >= 4,!.

%Vertical Check
check(X,Y):-
	traverse(X,Y,0,1,R1),
	traverse(X,Y,0,-1,R2),
	R is R1 + R2 - 1,
	R >= 4,!.

%Main Diagonal Check
check(X,Y):-
	traverse(X,Y,1,1,R1),
	traverse(X,Y,-1,-1,R2),
	R is R1 + R2 - 1,
	R >= 4,!.

%Secondary Diagonal Check
check(X,Y):-
	traverse(X,Y,1,-1,R1),
	traverse(X,Y,-1,1,R2),
	R is R1 + R2 - 1,
	R >= 4,!.

%%%%%%%%%  IO Rules %%%%%%%%%%%%%

write_piece(r):-write('x').
write_piece(y):-write('o').

print(P):-
	size(_,Y),
	print(1,Y,P).

print(C,R,P):-
	size(X,_),
	C == X,
	R == 1,
	piece(C,R,Color),
	write_piece(Color),
	write('  '),!.
print(C,R,P):-
	size(X,_),
	C == X,
	R == 1,
	not(piece(C,R,_)),
	write('.  '),!.

print(C,R,P):-
	size(X,Y),
	NewC is C+1,
	piece(C,R,Color),
	write_piece(Color),
	new(Circle,circle(20)),
    (Color == r -> send(Circle,fill_pattern,red) ; send(Circle,fill_pattern,yellow)),
	send(P,display(Circle,point(C * 30, (Y - R) * 30))),
	write('  '),
	C < X ,
	print(NewC,R,P),!.
print(C,R,P):-
	size(X,_),
	NewC is C+1 ,
	not(piece(C,R,_)) ,
	write('.  ') ,
	C < X   ,
	print(NewC,R,P),!.

print(_,R,P):-
	NewR is R-1 ,
	writeln(''),
	R >= 0,
	print(1,NewR,P).

scan(Color,P):-
	write('\nGive me column '),
	read(C),
	insert(C,Color),
	top(C,R),
	print(P),
	(checkStatus(C,R,Color),writeln('');!),!.
scan(Color,P):-
	writeln('Invalid'),scan(Color,P).

%%%%%%%%%%%  Tool Rules  %%%%%%%%%%%%

insert(C,Color):-
	size(_,R),
	top(C,H),
	H < R,
	TmpH is H + 1,
        retractall(top(C,_)),
        assert(top(C,TmpH)),
        assert(piece(C,TmpH,Color)),
	score(Color,V),
	eval(C,TmpH,E),
	V1 is V + E,
	retractall(score(Color,_)),
	assert(score(Color,V1));
	true.

remove(C):-
	top(C,H),
	piece(C,H,Color),
	retractall(piece(C,H,_)),
	TmpH is H - 1,
        retractall(top(C,_)),
        assert(top(C,TmpH)),
	score(Color,V),
	eval(C,H,E),
	V1 is V - E,
	retractall(score(Color,_)),
	assert(score(Color,V1));
	true.

%%%%%%%%  Evaluation Rules %%%%%%%%

createEvalTable():-
	size(_,R),
	retractall(eval(_,_,_)),
	initEvalRow(R),
	calcEvalRow(R).

initEvalCol(_,0):-!.
initEvalCol(CurRow,CurCol):-
	NewCol is CurCol - 1,
	initEvalCol(CurRow,NewCol),
	assert(eval(CurRow,CurCol,0)).

initEvalRow(0):-!.
initEvalRow(CurRow):-
	NewRow is CurRow - 1,
	initEvalRow(NewRow),
	size(C,_),
	initEvalCol(CurRow,C).

calcEvalCol(_,0):-!.
calcEvalCol(CurRow,CurCol):-
	NewCol is CurCol - 1,
	calcEvalCol(CurRow,NewCol),
	calcHor(CurRow,CurCol),
	calcVer(CurRow,CurCol),
	calcUpDia(CurRow,CurCol),
	calcDownDia(CurRow,CurCol).

calcEvalRow(0):-!.
calcEvalRow(CurRow):-
	NewRow is CurRow - 1,
	calcEvalRow(NewRow),
	size(C,_),
	calcEvalCol(CurRow,C).

calcHor(X,Y):-
	(Y1 is Y+1, Y2 is Y+2, Y3 is Y+3,
	eval(X,Y,E),eval(X,Y1,E1),eval(X,Y2,E2),eval(X,Y3,E3),
	Ne  is E+1 ,retractall(eval(X,Y,E))  ,assert(eval(X,Y,Ne)),
	Ne1 is E1+1,retractall(eval(X,Y1,E1)),assert(eval(X,Y1,Ne1)),
	Ne2 is E2+1,retractall(eval(X,Y2,E2)),assert(eval(X,Y2,Ne2)),
	Ne3 is E3+1,retractall(eval(X,Y3,E3)),assert(eval(X,Y3,Ne3)));
	true.

calcVer(X,Y):-
	(   X1 is X+1, X2 is X+2, X3 is X+3,
	eval(X,Y,E),eval(X1,Y,E1),eval(X2,Y,E2),eval(X3,Y,E3),
	Ne  is E+1 ,retractall(eval(X,Y,E))  ,assert(eval(X,Y,Ne)),
	Ne1 is E1+1,retractall(eval(X1,Y,E1)),assert(eval(X1,Y,Ne1)),
	Ne2 is E2+1,retractall(eval(X2,Y,E2)),assert(eval(X2,Y,Ne2)),
	Ne3 is E3+1,retractall(eval(X3,Y,E3)),assert(eval(X3,Y,Ne3)));
	true.

calcUpDia(X,Y):-
	(   X1 is X+1, X2 is X+2, X3 is X+3,
	Y1 is Y+1, Y2 is Y+2, Y3 is Y+3,
	eval(X,Y,E),eval(X1,Y1,E1),eval(X2,Y2,E2),eval(X3,Y3,E3),
	Ne  is E+1 ,retractall(eval(X,Y,E))  ,assert(eval(X,Y,Ne)),
	Ne1 is E1+1,retractall(eval(X1,Y1,E1)),assert(eval(X1,Y1,Ne1)),
	Ne2 is E2+1,retractall(eval(X2,Y2,E2)),assert(eval(X2,Y2,Ne2)),
	Ne3 is E3+1,retractall(eval(X3,Y3,E3)),assert(eval(X3,Y3,Ne3)));
	true.

calcDownDia(X,Y):-(
	X1 is X-1, X2 is X-2, X3 is X-3,
	Y1 is Y+1, Y2 is Y+2, Y3 is Y+3,
	eval(X,Y,E),eval(X1,Y1,E1),eval(X2,Y2,E2),eval(X3,Y3,E3),
	Ne  is E+1 ,retractall(eval(X,Y,E))  ,assert(eval(X,Y,Ne)),
	Ne1 is E1+1,retractall(eval(X1,Y1,E1)),assert(eval(X1,Y1,Ne1)),
	Ne2 is E2+1,retractall(eval(X2,Y2,E2)),assert(eval(X2,Y2,Ne2)),
	Ne3 is E3+1,retractall(eval(X3,Y3,E3)),assert(eval(X3,Y3,Ne3)));
        true.
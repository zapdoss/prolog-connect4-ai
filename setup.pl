score(r,0).
score(y,0).

%%%%%%%%  INIT GameBoard %%%%%%%%

init_top(0):-!.
init_top(C):-assert(top(C,0)),NewC is C-1, init_top(NewC) .

init(C,R,Picture):-
	retractall(size(_,_)),
	retractall(depth(_)),
	retractall(score(_,_)),
	retractall(strategy(_)),
	assert(score(r,0)),
	assert(score(y,0)),
	assert(size(C,R)),
	retractall(top(_,_)),
	init_top(C),
	initBoardGUI(C,R,Picture),
	retractall(piece(_,_,_)),
	resetStatus().

initBoardGUICol(0,_,_):- !.
initBoardGUICol(C,R,P):-
	NewC is C - 1,
	new(Circle,circle(20)),
    send(Circle,fill_pattern,white),
    send(P,display(Circle,point(C * 30, R * 30))),
	initBoardGUICol(NewC,R,P).

initBoardGUI(_,0,_):- !.
initBoardGUI(C,R,P):-
	NewR is R - 1,
	initBoardGUICol(C,NewR,P),
	initBoardGUI(C,NewR,P).

resetStatus():-
	retractall(win(_)),
	retractall(lose(_)).
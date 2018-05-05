:- use_module(library(pce)).
:- include('setup.pl').
:- include('ai.pl').
:- include('rules.pl').

:- dynamic top/2.
:- dynamic size/2.
:- dynamic piece/3.
:- dynamic win/1.
:- dynamic lose/1.
:- dynamic eval/3.
:- dynamic score/2.
:- dynamic depth/1.

%%%%%%%%%%%%  MAIN  %%%%%%%%%%%%%

newGame(C,R):-
	new(Picture, picture('Connect 4')),
	send(Picture, open),
	init(C,R,Picture),
	createEvalTable(),
	writeln('Select Computer Strategy [ enter 1 for AI or 2 for Greedy ]:'),
	writeln('Select depth of search tree: [ 1 - 5 ]'),
	read(D),
	assert(depth(D)),
	game(Picture).

game(P):-
	draw(),writeln('Draw Situation'),!.
game(P):-
	scan(r,P),
	win(r),
	writeln('You Win!'),!;
	writeln('\nComputers Turn:'),
	depth(D),
	doBest(y,D,_,Move),
	insert(Move,y),
	print(P),
	top(Move,H),
	(
	check(Move,H),writeln('\nComputer wins!');
			not(check(Move,H)),game(P)
	).
	
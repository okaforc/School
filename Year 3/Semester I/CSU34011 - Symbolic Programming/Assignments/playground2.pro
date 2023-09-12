snd(X, [_, X|_]).
snd(X, [_, X|T]) :- snd(X, T).

swap12([A, B|C], [B, A|C]).

tran(eins,one).
tran(zwei,two).
tran(drei,three).
tran(vier,four).
tran(fuenf,five).
tran(sechs,six).
tran(sieben,seven).
tran(acht,eight).
tran(neun,nine). 
lt([], []).
lt([H1|T1], [H2|T2]) :- tran(H1, H2), lt(T1, T2).

twice([], []).
twice([H|T], [H, H|L]) :- twice(T, L).
% twice([1, 2, 3], X).

% sl(X, Y) :- succ(X, Y).
sl(X, Y) :- Y is X+1.
ts(X, Y, Z) :- Z is X+Y.

addone([], []).
addone([H|T], [X|Y]) :- X is H + 1, addone(T, Y).

accMin([H|T], M, A) :-
    H < M,
    accMin(T, H, A).
accMin([H|T], M, A) :-
    M =< H,
    accMin(T, M, A).
accMin([], A, A).
amin([H|T], M) :- accMin([H|T], H, M).

scalarMult(X, [H|T], [A|B]) :-
    A is H*X,
    scalarMult(X, T, B).
scalarMult(_, [], []).

dotH([H1|T1], [H2|T2], X, Y) :-
    A is X + H1*H2,
    dotH(T1, T2, A, Y).
dotH([], [], X, X).
dot(X, Y, Z) :- dotH(X, Y, 0, Z).

accrev([], X, X).
accrev([H|T], A, X) :- accrev(T, [H|A], X).
rev(X, Y) :- accrev(X, [], Y).



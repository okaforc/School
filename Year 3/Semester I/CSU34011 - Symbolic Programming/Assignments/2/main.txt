% Part 1
% s([0,1,1,2|L],[]).
s --> [2].
s --> [1], s, [1].
s --> [0], s, [0].

% Part 2
% dnf


% Part 3
% 3.1
% accept([0,0,Z,0,0]).
tran(q0, X, q0) :- X is 0; X is 1.
tran(q0, 1, q1).
tran(q1, X, q2) :- X is 0; X is 1.
tran(q2, X, q3) :- X is 0; X is 1.
final(q3).

% 3.2
% q0([0,0,Z,0,0],[]).
q0 --> [0], q0.
q0 --> [1], q0.
q0 --> [1], q1.
q1 --> [0], q2.
q1 --> [1], q2.
q2 --> [0], q3.
q2 --> [1], q3.
q3 --> [].

% 3.3
% l3(String, succ(0)).
% l3(String, succ(succ(succ(succ(0))))).
l3(L, N) :- acc(q0, L, F, N), final(F).

acc(X, [], X, 0).
acc(A,[H|T],B,succ(X)) :- tran(A,H,C), acc(C,T,B,X).


accept(L) :- steps(q0,L,F), final(F).
steps(Q,[],Q).
steps(Q,[H|T],Q2) :- tran(Q,H,Qn), steps(Qn,T,Q2).

numeral(0).
numeral(succ(X)) :- numeral(X).

doubled(X) :- append(L, L, X).

suffix(S, L) :- append(_, S, L).
palindrome([]).
palindrome([_]).
palindrome([H|T]) :- suffix([H], [H|T]), rev(T, [_|B]), rev(B, C), palindrome(C).

toptail([_|T], Out) :- rev(T, [_|B]), rev(B, Out).

% toptail2([_|Tail], OutList):- rev(Tail, [_|TailReversedRest]), rev(TailReversedRest, OutList).

last1([H|T], X) :- rev([H|T], [X|_]).

last2([H|T], X) :- last2h([H|T], H, X).
last2h([], X, X).
last2h([H|T], _, X) :- last2h(T, H, X).

swapfl([], []).
swapfl([H1|T1], [H2|T2]) :- 
    rev(T2, [H1|X]),
    rev(T1, [H2|X]).
    
len(L, X) :- lenh(L, 0, X).
lenh([], X, X).
lenh([_|T], A, X) :-
    N is A + 1,
    lenh(T, N, X).


rev(L, R) :- revAcc(L, [], R).
revAcc([H|T], A, R) :- revAcc(T, [H|A], R).
revAcc([], A, A). 

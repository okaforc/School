subl([H|T], [H|S]) :- subl(T, S).
subl([H|T], [_|S]) :- subl([H|T], S).
subl([], _).

/* 
'[|]'(a,'[|]'(b,'[|]'(c,[])))  =  [a,b,c].   
'[|]'(a,'[|]'(b,'[|]'(c,[])))  =  [a,b|[c]].   
'[|]'('[|]'(a,[]),'[|]'('[|]'(b,[]),'[|]'('[|]'(c,[]),[])))  =  X.   
'[|]'(a,'[|]'(b,'[|]'('[|]'(c,[]),[])))  =  [a,b|[c]].
*/


% nonRep(X) :- nr(X, []).
% % nr([H1|T1], L) :- 
% nr([H1|T1], L) :- \+ member(H1, L), nr(T1, [H1|L]).


p(1).
p(2)  :-  !.
p(3). 

mem([H|_], H).
mem([_|T], X) :- mem(T, X).

fac(0, 1).
fac(N, F) :-
    N > 0,
    M is N - 1,
    fac(M, F1),
    F is F1 * N.

less(0, succ(_)).
less(succ(X), succ(Y)) :- less(X, Y).

% decrList([]).
decrList([_]).
decrList([H1,H2|T]) :- 
    less(H2, H1),
    decrList([H2|T]).

% sset(_, []).
sset(N, L) :- decrList([succ(N)|L]).
% sh(0, X, [0|X]).
% sh(succ(N), A, L) :-


moreThanOne(L) :- mto(L, []).
mto([H|T1], [H|T2]) :- member(H, D).
mto([H|T1], [H2|T2]) :- mto(T1, [H|T2]).
mto([], _).

hbs(0, [0]).
hbs(succ(0), [1]).
hbs(X, L) :- num(X, A), length(L, Len), hbsh(A, L, Len).

hbsh(0, [], X).
hbsh(X, [H|T], Len) :-
    L1 is Len - 1,
    V1 is 2 ^ L1,
    V2 = V1 * H,
    V3 = X - V2,
    hbsh(V3, T, L1).

num(0, A, A).
num(succ(X), A, N) :- 
    P is A + 1,
    num(X, P, N).
num(X, N) :- num(X, 0, N).


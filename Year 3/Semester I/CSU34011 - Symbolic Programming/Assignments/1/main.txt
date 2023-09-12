pterm(null).
pterm(f0(X)) :- pterm(X).
pterm(f1(X)) :- pterm(X).

% Part 1 %
incr(null, f1(null)).
incr(f0(X), f1(X)).
incr(f1(X), f0(Y)) :- incr(X, Y).


% Part 2 %
legal(f0(null)).
legal(X) :- legal(Y), incr(Y, X).

incrR(X, Y) :- legal(X), incr(X, Y).

% Part 3 %
% add(f1(null),f0(f1(null)),X). === X = f1(f1(null)) ;
%                                   no
add(X, f0(null), X).
add(X, Y, Z) :- incr(X, A), incr(B, Y), add(A, B, Z).

% Part 4 %
% mult(f1(null),f0(f1(null)),X). === X = f0(f1(null)) ;
%                                    no
mult(f0(null), _, f0(null)). % 0x = 0
mult(X, f1(null), X). % 1x = 1
mult(X, Y, Z) :- multHelper(X, Y, Z, X). 

multHelper(_, f0(null), f0(null), _). % 0x = 0
multHelper(X, f1(null), X, _). % 1x = x
multHelper(X, Y, Z, W) :- add(X, W, A), incr(B, Y), multHelper(A, B, Z, W). % 

% Part 5 %
% revers(f0(f1(null)),X). === X = f1(f0(null)) ;
%                             no
revers(null, _).
revers(f0(null), f1(null)).
revers(f1(null), f0(null)).
revers(X, Y) :- revHelp(X, null, Y).

revHelp(null, Y, Y).
revHelp(f0(X), Y, Z) :- revHelp(X, f0(Y), Z).
revHelp(f1(X), Y, Z) :- revHelp(X, f1(Y), Z).
% 

% Part 6 %
% normalize(f1(f0(f0(null))), X). === X = f1(null) ;
%                                     no
% normalize(null, f0(null)).
normalize(X, Y) :- revers(X, A), normHelp(A, Y).

normHelp(null, f0(null)).
normHelp(f0(A), Y) :- normHelp(A, Y).
normHelp(A, Y)  :- revers(A, Y).








% test add inputting numbers N1 and N2
testAdd(N1,N2,T1,T2,Sum,SumT) :- numb2pterm(N1,T1), numb2pterm(N2,T2),
add(T1,T2,SumT), pterm2numb(SumT,Sum).
% test mult inputting numbers N1 and N2
testMult(N1,N2,T1,T2,N1N2,T1T2) :- numb2pterm(N1,T1), numb2pterm(N2,T2),
mult(T1,T2,T1T2), pterm2numb(T1T2,N1N2).
% test revers inputting list L
% testRev(L,Lr,T,Tr) :- ptermlist(T,L), revers(T,Tr), ptermlist(Tr,Lr).
% % test normalize inputting list L
% testNorm(L,T,Tn,Ln) :- ptermlist(T,L), normalize(T,Tn), ptermlist(Tn,Ln).
% make a pterm T from a number N numb2term(+N,?T)
numb2pterm(0,f0(null)).
numb2pterm(N,T) :- N>0, M is N-1, numb2pterm(M,Temp), incr(Temp,T).
% make a number N from a pterm T pterm2numb(+T,?N)
pterm2numb(null,0).
pterm2numb(f0(X),N) :- pterm2numb(X,M), N is 2*M.
pterm2numb(f1(X),N) :- pterm2numb(X,M), N is 2*M +1.
% reversible ptermlist(T,L)
ptermlist(null,[]).
ptermlist(f0(X),[0|L]) :- ptermlist(X,L).
ptermlist(f1(X),[1|L]) :- ptermlist(X,L).
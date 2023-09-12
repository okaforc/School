arc([H|T], Node, Cost, KB) :- 
    member([H|B], KB), 
    append(B, T, Node),
    length(B, L), 
    Cost is 1 + L/(L + 1).

heuristic(Node, H) :- length(Node, H).

goal([]).

less_than([[Node1|_], Cost1], [[Node2|_], Cost2]) :-
    heuristic(Node1, Hvalue1), 
    heuristic(Node2, Hvalue2),
    F1 is Cost1 + Hvalue1, 
    F2 is Cost2 + Hvalue2,
    F1 =< F2.

add2frontier(X, Y, Z) :-
    append(X, Y, A),
    bsort(A, Z).


sumCost([], _, []).
sumCost([[N1, P1, C1]|T], Cost, NC) :- 
    sumCost(T, Cost, A),
    C2 is C1 + Cost,
    append([[N1, P1, C2]], A, NC).

    
bsort([], []).
bsort([H|T], X) :-
    swap([H|T], [NL]),
    !,
    bsort(NL, X).
bsort(X, X).

min_value([], Min, Min).
min_value([H|T], CurrMin, Min):- less_than(H, CurrMin), !, NewMin = H, min_value(T,NewMin,Min).
min_value([H|T], CurrMin,Min):- less_than(CurrMin,H), min_value(T, CurrMin, Min).

prioritise([Head|Rest], [Min|ListMinRemoved]):-
    min_value(Rest, Head, Min), 
    select(Min, [Head|Rest], ListMinRemoved).

swap([[N1, P1, C1], [N2, P2, C2]|T], [[N2, P2, C2], [N1, P1, C1]|T]) :-
    append(P1, N1, P3), 
    append(P2, N2, P4), 
    less_than([P4, C2],[P3, C1]),
    !.
swap([X|T1], [X|T2]) :- swap(T1, T2).

search([[Node, PathA, Cost]|_], PathB, Cost, _) :- goal(Node), append(PathA, [Node], PathB).
search([[Node, PathA, CostA]|More], PathB, CostB, KB) :- 
    append(PathA, [Node], New),
    findall([X, New, Y], arc(Node, X, Y, KB), Succs),
    sumCost(Succs, CostA, NSucc),
    add2frontier(NSucc, More, NF),
    !,
    search(NF, PathB, CostB, KB).

% astar([q], Path, Cost, [[q,a],[q,b,c],[a,d,e],[a,c,e,f],[b,c],[c,e,f],[e],[f,e]]).
astar(Node, Path, Cost, KB) :- search([[Node, [], 0]], Path, Cost, KB).


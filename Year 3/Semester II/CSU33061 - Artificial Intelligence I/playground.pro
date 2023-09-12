arc([H|T],Node,Cost,KB) :- member([H|B],KB), append(B,T,Node),
length(B,L), Cost is 1+ L/(L+1).
heuristic(Node,H) :- length(Node,H).
goal([]).
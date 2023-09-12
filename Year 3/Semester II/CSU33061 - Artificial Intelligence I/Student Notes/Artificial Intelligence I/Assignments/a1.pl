% astar(+Node, ?Path, ?Cost, +KB)
astar(Node, Path, Cost, KB):- search([[Node,[],0]], Path, Cost, KB).

% search(+[[Node, Path, Cost]|Tail], -Path, -Cost, +KB)
search([[Node, Path, Cost]|_], FinalPath, Cost, _):- goal(Node), append(Path, [[]],FinalPath).
search([[Node, Path, Cost]|Tail], OverallPath, OverallCost, KB):-
    append(Path, [Node], NewPath),
    % Find all border nodes
    findall([Next,NewPath,CostToNode],arc(Node,Next,CostToNode,KB),Children),
    % Add costs to Children
    add_node_costs(Children, Cost, NewChildren),
    % Add all border nodes to frontier
    append(NewChildren, Tail, Frontier),
    % Sort frontier
    prioritise_frontier(Frontier,Sorted),
    % Recursively search
    search(Sorted,OverallPath,OverallCost,KB).

% Add costs(+Children, +CostToParent, -ChildrenWithCostAdded)
add_node_costs([], _, []).
add_node_costs([[Node,Path,Cost]|T], CostToParent, NewChildren):- 
    add_node_costs(T,CostToParent,Rest), 
    CombinedCost is CostToParent+Cost, 
    append([[Node,Path,CombinedCost]],Rest,NewChildren).

% less_than(+PathCostPair1, +PathCostPair2)
less_than([Path1,Cost1], [Path2,Cost2]):-
    heuristic(Path1, Hvalue1), heuristic(Path2, Hvalue2),
    F1 is Cost1+Hvalue1, F2 is Cost2+Hvalue2,
    F1 =< F2.

% heuristic gives the estimated cost from a node to the goal
heuristic(Node, H):- length(Node, H).

% arc
arc([H|T], Node, Cost, KB):- 
    member([H|B],KB), 
    append(B,T,Node),
    length(B,L),
    Cost is 1 + (L/(L+1)).

% goal
goal([]).

% SORTING FUNCTIONS - quicksort is likely the fastest sorting method, although that is not entirely necessary
% prioritise_frontier(+List, -SortedList)
prioritise_frontier(Frontier, Sorted):- quick_sort_frontier(Frontier, [], Sorted).

% quick sort
quick_sort_frontier([], Acc, Acc).
quick_sort_frontier([Head|Tail], Acc, Sorted):-
    pivot_frontier(Head, Tail, List1, List2),
    quick_sort_frontier(List1,Acc,Sorted1), 
    quick_sort_frontier(List2,[Head|Sorted1],Sorted).

% Split the frontier into 2 halves recursively sorting 
pivot_frontier(_, [], [], []). % Base case

% Less than or equal case
pivot_frontier([Node1, Path1, Cost1], [[Node2, Path2, Cost2]|Tail], [[Node2, Path2, Cost2]|List1], List2):- 
    append(Path1,Node1,NewPath1), append(Path2, Node2, NewPath2),
    less_than([NewPath1,Cost1],[NewPath2,Cost2]), !, 
    pivot_frontier([Node1, Path1, Cost1], Tail, List1, List2).

% Greater than case
pivot_frontier([Node1, Path1, Cost1], [[Node2, Path2, Cost2]|Tail], List1, [[Node2, Path2, Cost2]|List2]):- 
    append(Path1,Node1,NewPath1), append(Path2, Node2, NewPath2),
    less_than([NewPath2,Cost2], [NewPath1,Cost1]),
    pivot_frontier([Node1, Path1, Cost1], Tail, List1, List2).
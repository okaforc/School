## Code to copy
### Goal 
#### FSM
```prolog
% goal(Node, Next, SetFinalStates)
goal(Q,[],Final):- member(Q,Final).
```
The goal has been reached if Next is empty, and Node is a member of the Final states.
#### Prolog
```prolog
goal([]).
```

### Arc
#### arc/3
```prolog
% arc(Node1,Node2,KnowledgeBase)
arc([H|T],Next,KB) :- member([H|B],KB), append(B,T,Next).
```
(Clause in knowledge base with that head.)
- Node 1 is the current node, seperated into its head and tail
- Node 2 is the next node
- member finds if node 1's head starts any of KB's elements, and returns the rest of the element that head starts as B
- Then, the the rest of KB's element (excluding the head) is concatenated with Node 1's tail, and is returned as Next
#### arc/2
```prolog
arc(Node1, Node2).
```
probably
##### Note
```arc([p,q],[q],[[p]]) and arc([i],[p,q],[[i,p,q]])```


### BFS (FIFO)
![[Pasted image 20230203142610.png]]
Corresponds directly to determinisation, applying powerset construction to search.
Adds newly discovered elements to end of frontier
```prolog
add2frontier(Children,[],Children). 

add2frontier(Children,[H|T],[H|More]) :- 
	add2frontier(Children,T,More).
```
#### add2frontier BFS
```prolog
add2frontier(Children, Rest, Frontier) :- append(Rest, Children, NewFrontier).
```

Adds newly discovered elements to end of frontier (back of queue).

### DFS (LIFO)
![[Pasted image 20230203142720.png]]
Go all the way down then backtrack. 
Add newly discovered children to start of frontier.
```prolog
add2frontier([],Rest,Rest). 

add2frontier([H|T],Rest,[H|TRest]) :- 
	add2frontier(T,Rest,TRest).
```
#### add2frontier DFS 
```prolog
add2frontier(Children, Rest, Frontier) :- append(Children, Rest, NewFrontier).
```
Add newly discovered children to start of frontier (top of stack).




### Frontier Search
#### DFS
##### prove
```prolog
prove([],_). % goal([]).
prove(Node,KB) :- arc(Node,Next,KB), prove(Next,KB).
```
- If the node is empty, then it has been proved
- Otherwise, find the next node, and check that
##### fs
```prolog
% fs(NodeList, KnowledgeBase)
fs([[]|_],_).

fs([[cut|T]|_], KB) :- fs([T],KB).

fs([Node|More], KB) :-
	Node = [H|_], H\== cut,
	findall(Next, arc(Node,Next,KB), Children),
	append(Children, More, NewFrontier),
	fs(NewFrontier, KB).
```
#### BFS
```prolog
% fs(NodeList, KnowledgeBase)
fs([[]|_],_).

fs([[cut|T]|_], KB) :- fs([T],KB).

fs([Node|More], KB) :-
	Node = [H|_], H\== cut,
	findall(Next, arc(Node, Next, KB), Children),
	append(More, Children, NewFrontier),
	fs(NewFrontier, KB).
```
#### Then
`if(p,q,r) :- (p, cut, q); r. % contra(p,q); r` 
and
`negation-as-failure(r):- (p,cut,fail); true.`

### A*
1. Create a function which compares the F-value (combination of cost and heuristic) of 2 two nodes:
```prolog
less_than(Node1,Node2) :- 
    cost(Node1, Cost1), cost(Node2, Cost2),
    heuristic(Node1,H1), heuristic(Node2, H2),
    F1 is Cost1 + H1, F2 is Cost2 + H2,
    F1 =< F2.
```
2. Define a prioritisation function, makes the first element of the list be the min.
```prolog
min_val([],Min,Min).
min_val([H|Rest],CurrMin,Min):- less_than(H, CurrMin), !, min_val(Rest,H, Min).
min_val([H|Rest],CurrMin,Min):- less_than(CurrMin, H), min_val(Rest,CurrMin, Min).

prioritise([Head|Rest], [Min|MinRemoved]):-
    min_val(Rest, Head, Min), select(Min, [Head|Rest], MinRemoved).
```
3. add2frontier
```prolog
add2frontier(Children, Rest, NewFrontier):-
    append(Children, Rest, Frontier),
    prioritise(Frontier, NewFrontier).
```
4. Modify highest level search
```prolog
astar([Node|_],_):- goal(Node).
astar([Node|Rest], KB):-
    findall(Next, arc(Node,Next,KB), Children),
    add2frontier(Children,Rest,NewFrontier),
    astar(NewFrontier, KB).
```
## Definitions
### Costs
**The value of an arc.**
You can define a cost by whatever you want - eg: time, space, money.
This is the Australia map given at [[2. Frontier Search - Blind Searches (BFS,DFS)#Graph modelling]]
![[Pasted image 20230418152803.png]]

#### How to find costs
Given above, how do we find the cost to get from `WA` to `NSW` (note that the option given here is not the only route!)?
	cost(`WA`, `NT`, `Q`, `NSW`) = 1 + 2 + 2 = 5

In general, the cost is the individual sum of costs along the path (i.e., it is *NOT* a running sum).
![[Pasted image 20230418153149.png]]

### Heuristics
**A problem-solving strategy or method that is not guaranteed to find the optimal solution, but is designed to find a satisfactory solution in a reasonable amount of time**.
h(`Node`) is the heuristic function.
h(`Node`) = estimate the minimum cost of a path from `Node` to a goal node

Often, ends up being an estimate assuming a lot of arcs, simplifying the problem (eg an arc between every node, so there is an easy quick route)

#### Examples of heuristics
##### FSM
FSM accept, where node = `[Q, String]`, and every arc costs 1.
	h(`[Q,String]`) = length(String)

##### Prolog search
Prolog search where node = `List of Propositions to prove`, and every arc costs 1
	h(`List`) = length(`List`)

##### Euclidean Plane
Where node = `point on Euclidean plane`, and cost = `distance between nodes`, and goal is a point `G`
	h(`Node`) = straight line distance to `G`

### Admissability
**A-star is admissible (under `cost,h`) if it returns a solution of minimum cost whenever it exists.**

3 sufficient (not necessary) conditions for admissability:
1. **Under-estimate:** 
	- For every solution $n...goal$:
	- $0 \leq$ `h(n)` $\leq$ `cost(n...goal)`
	- (Heuristic is always less than cost)

2. **Termination:** 
	- For some $\epsilon > 0$, every arc costs $\geq \epsilon$
	- All arcs are positive, so if a path is sufficiently long, it will eventually exceed another cost and terminate that path

3. **Finite branching:** 
	- $\{n'|arc(n,n')\}$ is finite for each node n
	- There are no nodes with infinite arcs

#### Proof: A-star returns a solution with min cost c
Assuming the conditions above, let p be a solution.

We are trying to show that A-star returns a solution with minimum cost c

Let 
1. $F_0=[Start]$, 
2. $F_{n+1}$ be $A^*$'s next frontier after $F_n$  ([] if none)
3. $c_n$ be the cost of the head of $F_n$ ($\infty$ if $F_n=[]$).

Then,
1. For all $n \geq 0$ such that $c_{n} <c, F_n$ has a prefix of p
2. $c = c_n$ for some $n$ such that the head of $F_n$ is a solution


#### BFS
BFS is admissible because:
1. it is just A* with heuristic = 0 (min-cost search) - under-estimation
2. it has all arcs = 1 - termination
3. given finitely many arcs, it would satifsy - finite branching

#### DFS
DFS is *NOT* admissible
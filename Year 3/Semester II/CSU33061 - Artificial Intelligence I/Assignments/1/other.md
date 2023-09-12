Assignment notes!!!!!!!
### 1. Some form of sort function!!!!
I did quick sort, jsut google it lol, you will need to modify whatever it is to deal with the type of list.

### 2. astar(+Node, ?Path, ?Cost, +KB)
Node is just going to be a list with one element - eg [q]
Path is going to be outputted 
Cost is going to be outputting
KB is knowledge base, stored in the form like [[q,a],[q,b,c],[a,d,e],[a,c,e,f],[b,c],[c,e,f],[e],[f,e]]
(look at ass sheet for more info lol)

this function should just call function 3

### 3. search(+[[Node, Path, Cost]|Tail], -Path, -Cost, +KB)
The order of node path cost doesnt matter i stole mine from shi su
Shi su called this astar2, but it's based on the search thing in the assignment sheet so i just called it search and u can too
Have a base case based on search([Node|_]) :- goal(Node).

Then, in the non-base case:

#### 1. Add the current Node to the ongoing Path 
Note that the path should eventually take the format like [[q], [a], [c, e, f], [e, f, e, f], [f, e, f], [e, e, f], [e, f], [f], [e], []]

#### 2. Find all the border nodes
You can take this pretty closely from his example search code, but you'll need to get the returned nodes into the format [Node,Path,Cost] or whatever order you've chosen.

#### 3. Add the cost from the start node to all border nodes

#### 4. Append border nodes (with updated cost) and Existing Frontier (the tail)

#### 5. Sort the new frontier from the last step

#### 6. Recursively search


### Also all the ones you just copy from the assignment
1. arc
2. heuristic
3. goal
4. less-than (you will just use this for the search function!!!!!!!! Make sure to use this for the search function!!!!) 

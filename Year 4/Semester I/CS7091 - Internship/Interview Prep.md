## Clothing
Top:
- [x] Sweater over tucked-in shirt
Bottom:
- [x] Straight black trousers OR black jeans
Shoes:
- [x] Brown loafers OR black dress shoes

## Object-Oriented Programming
Object-oriented programming (OOP) is a programming paradigm making use of objects, which can contain fields and methods. These are popularly class-based, meaning that objects are instances of classes.

### Class
A class is a program template for creating instances of objects. The values of fields in a class are the default values for any instances of that class.

### Instance
An instance is a copy or an occurrence of a class or other object. They are created by an instantiation of a class.

### Object
An instance of a class. An object has a [[#type]], but implements a [[#class]].

### Type
A (data) type is a collection or group of data values, typically bounded by 
- the set of possible values the type can have, 
- the operations allowed on values in the set, and/or
- a representation of these values as machine types

### Class vs Type
A [[#class|classes]] defines how an object is implemented. Classes define the internal state and implementation of its operations.
A [[#type]] is the interface of an object. Types define the set of requests an object can respond to.

### Encapsulation
Encapsulation is 
- the bundling of fields and methods (data and/or methods that operate on data)
- a method of restricting access to the data in the object
If restricting, data can only be accessed in approved methods, such as "set" and "get" methods to prevent violating state invariance. Examples of encapsulation are visibility modifiers (public, private, protected) on member variables and methods of [[#class|classes]].

### Variance

### Interface
An interface is a data type that contains only declarations of methods that can be implemented by a class. It cannot be instantiated and must make use of all methods in the interface.

### Abstract modifier
The `abstract` modifier means that the thing being modified has a missing or incomplete definition. Abstract classes cannot be instantiated. Classes that inherit an abstract class can either implement or override its methods, or choose to do neither. Functions in an abstract class are not necessarily abstract. Any member variables or functions marked as `abstract` must be implemented in the class that inherits the abstract class. 

### Virtual modifier
The `virtual` modifier means that a method or variable can be overridden. It can be used in regular (non-abstract or -interface) classes, and therefore can have an implementation. This gives the developer the option to choose whether or not to override a field or function, but it isn't necessary.


### `inherits`
Java keyword to implement an [[#interface]].
### `extends`
Java keyword to implement a [[#class]].

### Abstraction
Abstraction is the process of removing or generalizing details to make a concept easier to understand. Usually, this is hiding lower level machine work such as compiling to machine code, or the work of transistors on a motherboard. It's about ignoring irrelevant features in favour of essential ones. Examples of abstraction are use of [[#Abstract modifier]], [[#Interface]], and [[#Inheritance]].

### Inheritance
Inheritance is the act of basing the implementation of a class or object upon another class or object to retain some or all of its implementation.
#### C\#
In C#, interfaces are inherited after the initial class and a colon. They must be implemented after the inherited base class.
```c#
public abstract class SomethingHere
{
    public int p = 3;
    public abstract void Apple(); // abstract members cannot be defined, only declared.
}

public interface ISomethingThird
{
	// whatever
}

// The class SomethingHere comes first, then the interface ISomethingThird
public class SomethingElse : SomethingHere, ISomethingThird
{
    public new int p = 4;

    public override void Apple()
    {
		// whatever else
    }
}
```


### Polymorphism
Polymorphism is the idea that you can access objects of different types through the same interface.
#### Ad Hoc
Ad hoc polymorphism (also sometimes called static polymorphism) is the idea that different functions can be applied to arguments of different types or quantity. This allows for operator and method overloading.

##### Example
```c#
public int Sum(int a, int b) {
	return a + b;
}

public double Sum(double a, double b) {
	return a + b;
}
```

The above is an example of both types of overloading; the method `Sum` can either take in two `ints` or two `doubles`, and return different types, even though they have the same name. They don't need to return different types, either, but they do here. This is *method overloading*.

Additionally, the `+` operator is shown here being used for both `ints` and `doubles` without need to specify or convert anything. This is *operator overloading*. The below is also an example of operator overloading, this time in Python:
```python
a = 1 + 5 
# a = 6

b = "1" + "5"
# b = "15"
```

Python does not directly support method overloading.

#### Parametric
Parametric polymorphism allows for using a single piece of code to be given a generic type instead of an actual concrete type, then then instantiated with specific types as needed.

A simple idea of this is the identity function: a function that returns the parameter it's given, unmodified. Without parametric polymorphism, the programmer would need to specify the input and return type of the parameter if they ever wanted to call it.

##### Example
```c#
List<int> = new();
List<string> = new();
List<List<List<int>>> = new();
```
The `List<T>` type uses the generic `T` in its creation. C#, Java, and Python support parametric polymorphism.

#### Subtype
Subtype polymorphism allows for the replacing of subtypes with supertypes. In other words, a subtype $S$ of supertype $T$ can be safely used in whatever context $T$ is expected.

Both C# and Java support subtyping, as every object in either language inherit from their respective `Object` class.

### First-class citizen
Any data type that can be:
- passed as an argument to a function,
- returned from a function,
- assigned to a variable

## Software Development Life Cycle (SDLC)
SDLC is **the cost-effective and time-efficient process that development teams use to design and build high-quality software**.
1. **Planning:** Defining the scope and priorities of the project
2. **Analysis**: 

## Agile
Agile is the development process of making iterative and incremental software updates. It prioritises teamwork and collaborativity. It is split into six stages:
1. **Requirements**: Defining the scope and priorities of the project
2. **Design**: Designing the project according to the above requirements
3. **Development**: Developing and creating iterations of the project from the design
4. **Testing**: Testing and quality-checking the project before deployment
5. **Deployment**: Releasing the current iteration of the project 
6. **Review**: Taking in feedback from the project's release and using it to define the scope of the next iteration
7. **Launch**: Launching the major version of the project once all functionalities have been created

## Data structures
|Data Structure|Time Complexity|   |   |   |   |   |   |   |Space Complexity|
|---|---|---|---|---|---|---|---|---|---|
||Average|   |   |   |Worst|   |   |   |Worst|
||Access|Search|Insertion|Deletion|Access|Search|Insertion|Deletion||
|[Array](http://en.wikipedia.org/wiki/Array_data_structure)|`Θ(1)`|`Θ(n)`|`Θ(n)`|`Θ(n)`|`O(1)`|`O(n)`|`O(n)`|`O(n)`|`O(n)`|
|[Stack](http://en.wikipedia.org/wiki/Stack_(abstract_data_type))|`Θ(n)`|`Θ(n)`|`Θ(1)`|`Θ(1)`|`O(n)`|`O(n)`|`O(1)`|`O(1)`|`O(n)`|
|[Queue](http://en.wikipedia.org/wiki/Queue_(abstract_data_type))|`Θ(n)`|`Θ(n)`|`Θ(1)`|`Θ(1)`|`O(n)`|`O(n)`|`O(1)`|`O(1)`|`O(n)`|
|[Singly-Linked List](http://en.wikipedia.org/wiki/Singly_linked_list#Singly_linked_lists)|`Θ(n)`|`Θ(n)`|`Θ(1)`|`Θ(1)`|`O(n)`|`O(n)`|`O(1)`|`O(1)`|`O(n)`|
|[Doubly-Linked List](http://en.wikipedia.org/wiki/Doubly_linked_list)|`Θ(n)`|`Θ(n)`|`Θ(1)`|`Θ(1)`|`O(n)`|`O(n)`|`O(1)`|`O(1)`|`O(n)`|
|[Skip List](http://en.wikipedia.org/wiki/Skip_list)|`Θ(log(n))`|`Θ(log(n))`|`Θ(log(n))`|`Θ(log(n))`|`O(n)`|`O(n)`|`O(n)`|`O(n)`|`O(n log(n))`|
|[Hash Table](http://en.wikipedia.org/wiki/Hash_table)|`N/A`|`Θ(1)`|`Θ(1)`|`Θ(1)`|`N/A`|`O(n)`|`O(n)`|`O(n)`|`O(n)`|
|[Binary Search Tree](http://en.wikipedia.org/wiki/Binary_search_tree)|`Θ(log(n))`|`Θ(log(n))`|`Θ(log(n))`|`Θ(log(n))`|`O(n)`|`O(n)`|`O(n)`|`O(n)`|`O(n)`|
|[Cartesian Tree](https://en.wikipedia.org/wiki/Cartesian_tree)|`N/A`|`Θ(log(n))`|`Θ(log(n))`|`Θ(log(n))`|`N/A`|`O(n)`|`O(n)`|`O(n)`|`O(n)`|
|[B-Tree](http://en.wikipedia.org/wiki/B_tree)|`Θ(log(n))`|`Θ(log(n))`|`Θ(log(n))`|`Θ(log(n))`|`O(log(n))`|`O(log(n))`|`O(log(n))`|`O(log(n))`|`O(n)`|
|[Red-Black Tree](http://en.wikipedia.org/wiki/Red-black_tree)|`Θ(log(n))`|`Θ(log(n))`|`Θ(log(n))`|`Θ(log(n))`|`O(log(n))`|`O(log(n))`|`O(log(n))`|`O(log(n))`|`O(n)`|
|[Splay Tree](https://en.wikipedia.org/wiki/Splay_tree)|`N/A`|`Θ(log(n))`|`Θ(log(n))`|`Θ(log(n))`|`N/A`|`O(log(n))`|`O(log(n))`|`O(log(n))`|`O(n)`|
|[AVL Tree](http://en.wikipedia.org/wiki/AVL_tree)|`Θ(log(n))`|`Θ(log(n))`|`Θ(log(n))`|`Θ(log(n))`|`O(log(n))`|`O(log(n))`|`O(log(n))`|`O(log(n))`|`O(n)`|
|[KD Tree](http://en.wikipedia.org/wiki/K-d_tree)|`Θ(log(n))`|`Θ(log(n))`|`Θ(log(n))`|`Θ(log(n))`|`O(n)`|`O(n)`|`O(n)`|`O(n)`|`O(n)`|

![[Pasted image 20231005223141.png]]

### Array
The array is a data structure consisting of a collection of elements of the same memory size. Each value in an array has at least one array index or key. For an array stored at a certain memory address, you can get the position of any value in the array by adding the initial address to the index you want plus the size of the object in byes. For example,
For an array of 32-bit integers (4 bytes) stored at memory address 2000, if a programmer wants the value at index $i$, the value can be calculated as
$$2000 + [i \times 4]$$
### List
A list (also known as a *dynamic array*) is an array whose memory can be resized. Usually, this includes use of a capacity value which, when the actual number of values in the list breaches, increases in size. Because of how expensive the memory allocation function may be, it might be beneficial to space out how often it is used. As a result, some list implementations may double the capacity when breached.

### Linked List
A linked list is a list where the elements of the list aren't ordered sequentially by element size, but rather by pointing to its next element. This allows for quick insertion and deletion of nodes, as the entire list needn't be restructured to add or remove an element in the middle of the list. However, this is dependent on having access to the target link, as otherwise traversal up the list is required.

A linked list is better over an array because it has faster worst-case insertion and deletion. It is worse because it has slower worst-cast access.

#### Reversing a linked list
1. Create 3 temporary nodes `prev` = null, `curr` = `head`, `next` = null
2. Point `next` to `curr.next` 
3. Point `curr.next` to `prev`
4. Point `prev` to `curr`
5. Point `curr` to `next`
6. Repeat until `curr` is null
7. Set the last node as the root

### Doubly-Linked List
A DLL is a linked list where each node can also point to its previous node.

### Tree
A tree is a hierarchal data structure where each node in the tree is either the parent of child of another node. Each node has exactly 1 parent, except the root node, which has only children. Because of this, loops are not possible in trees.

#### Binary Tree 
A binary is a tree in which the number of children is restricted to at most 2. A binary tree is considered ***complete*** if all rows (except maybe the last one) are filled. If the last row is not filled, then it must be filled left to right. 

#### Binary Search Tree
A BST is a binary tree in which for all nodes, the key of that node is greater than all values in its left subtree and less than all values in its right subtree.

#### Heap
A heap is a tree that satisfies the *heap property*:
- in a max-heap, for any node, that node's value is greater than or equal to the value of any of its children
- in a min-heap, for any node, that node's value is less than or equal to the value of any of its children
See an example of a complete min-heap below.
![[330px-Min-heap.png|500]]
## Sorting algorithms
|Algorithm|Time Complexity|   |   |Space Complexity|
|---|---|---|---|---|
||Best|Average|Worst|Worst|
|[Quicksort](http://en.wikipedia.org/wiki/Quicksort)|`Ω(n log(n))`|`Θ(n log(n))`|`O(n^2)`|`O(log(n))`|
|[Mergesort](http://en.wikipedia.org/wiki/Merge_sort)|`Ω(n log(n))`|`Θ(n log(n))`|`O(n log(n))`|`O(n)`|
|[Timsort](http://en.wikipedia.org/wiki/Timsort)|`Ω(n)`|`Θ(n log(n))`|`O(n log(n))`|`O(n)`|
|[Heapsort](http://en.wikipedia.org/wiki/Heapsort)|`Ω(n log(n))`|`Θ(n log(n))`|`O(n log(n))`|`O(1)`|
|[Bubble Sort](http://en.wikipedia.org/wiki/Bubble_sort)|`Ω(n)`|`Θ(n^2)`|`O(n^2)`|`O(1)`|
|[Insertion Sort](http://en.wikipedia.org/wiki/Insertion_sort)|`Ω(n)`|`Θ(n^2)`|`O(n^2)`|`O(1)`|
|[Selection Sort](http://en.wikipedia.org/wiki/Selection_sort)|`Ω(n^2)`|`Θ(n^2)`|`O(n^2)`|`O(1)`|
|[Tree Sort](https://en.wikipedia.org/wiki/Tree_sort)|`Ω(n log(n))`|`Θ(n log(n))`|`O(n^2)`|`O(n)`|
|[Shell Sort](http://en.wikipedia.org/wiki/Shellsort)|`Ω(n log(n))`|`Θ(n(log(n))^2)`|`O(n(log(n))^2)`|`O(1)`|
|[Bucket Sort](http://en.wikipedia.org/wiki/Bucket_sort "Only for integers. k is a number of buckets")|`Ω(n+k)`|`Θ(n+k)`|`O(n^2)`|`O(n)`|
|[Radix Sort](http://en.wikipedia.org/wiki/Radix_sort "Constant number of digits 'k'")|`Ω(nk)`|`Θ(nk)`|`O(nk)`|`O(n+k)`|
|[Counting Sort](https://en.wikipedia.org/wiki/Counting_sort "Difference between maximum and minimum number 'k'")|`Ω(n+k)`|`Θ(n+k)`|`O(n+k)`|`O(k)`|
|[Cubesort](https://en.wikipedia.org/wiki/Cubesort)|`Ω(n)`|`Θ(n log(n))`|`O(n log(n))`|`O(n)`|

![[Pasted image 20231005230808.png|500]]

#### Bubble Sort
```c#
public static List<int> bubbleSort(List<int> nums)
{
    bool sorted = false;
    while (!sorted)
    {
        sorted = true;
        for (int i = 0; i < nums.Count - 1; i++)
        {
            if (nums[i] > nums[i + 1])
            {
                sorted = false;
                (nums[i+1], nums[i]) = (nums[i], nums[i+1]);
            }
        }
    }
    return nums;
}
```

#### Merge Sort
```c#
public int[] SortArray(int[] array, int left, int right)
{
  if (left < right)
  {
    int middle = left + (right - left) / 2;

    SortArray(array, left, middle);
    SortArray(array, middle + 1, right);

    MergeArray(array, left, middle, right);
  }

  return array;
}

public void MergeArray(int[] array, int left, int middle, int right)
{
  var leftArrayLength = middle - left + 1;
  var rightArrayLength = right - middle;
  var leftTempArray = new int[leftArrayLength];
  var rightTempArray = new int[rightArrayLength];
  int i, j;

  for (i = 0; i < leftArrayLength; ++i)
    leftTempArray[i] = array[left + i];
  for (j = 0; j < rightArrayLength; ++j)
    rightTempArray[j] = array[middle + 1 + j];

  i = 0;
  j = 0;
  int k = left;

  while (i < leftArrayLength && j < rightArrayLength)
  {
    if (leftTempArray[i] <= rightTempArray[j])
    {
      array[k++] = leftTempArray[i++];
    }
    else
    {
      array[k++] = rightTempArray[j++];
    }
  }

  while (i < leftArrayLength)
  {
    array[k++] = leftTempArray[i++];
  }

  while (j < rightArrayLength)
  {
    array[k++] = rightTempArray[j++];
  }
}
```
## Searching algorithms
### Binary Search
#### Iterative
```c#
public int bSearch(int[] nums, int target) {
	int lo = 0, hi = nums.Length - 1;
	while (lo < hi) {
		int mid = lo + (hi - lo)/2;
		if (target < nums[mid]) hi = mid - 1;
		else if (target > nums[mid]) lo = mid + 1;
		else return nums[mid];
	}
	return -1; // not found
}
```

#### Recursive
```c#
public int bSearch(int[] nums, int target) {
	return bSearchHelper(nums, target, 0, nums.Length - 1);
}

int bSearchHelper(int[] nums, int target, int lo, int hi) {
	if (lo > hi) return -1;

	int mid = lo + (hi - lo)/2;
	if (target < nums[mid]) return bSearchHelper(nums, target, lo, mid - 1);
	else if (target > nums[mid]) return bSearchHelper(nums, target, lo + 1, hi);
	else return nums[mid];
}
```

## Languages
### C\#

### Java

### Python

### C

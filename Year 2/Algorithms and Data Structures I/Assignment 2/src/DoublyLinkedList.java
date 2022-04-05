import java.util.Iterator;
import java.util.ListIterator;
import java.util.NoSuchElementException;

// -------------------------------------------------------------------------
/**
 *  This class contains the methods of Doubly Linked List.
 *
 *  @author  Chike Okafor
 *  @version 09/10/18 11:13:22
 */

/**
 * Class DoublyLinkedList: implements a *generic* Doubly Linked List.
 * 
 * @param <T> This is a type parameter. T is used as a class name in the
 *            definition of this class.
 *
 *            When creating a new DoublyLinkedList, T should be instantiated
 *            with an actual class name that extends the class Comparable. Such
 *            classes include String and Integer.
 *
 *            For example to create a new DoublyLinkedList class containing
 *            String data: DoublyLinkedList<String> myStringList = new
 *            DoublyLinkedList<String>();
 *
 *            The class offers a toString() method which returns a
 *            comma-separated sting of all elements in the data structure.
 * 
 *            This is a bare minimum class you would need to completely
 *            implement. You can add additional methods to support your code.
 *            Each method will need to be tested by your jUnit tests -- for
 *            simplicity in jUnit testing introduce only public methods.
 */
class DoublyLinkedList<T extends Comparable<T>> {

    /**
     * private class DLLNode: implements a *generic* Doubly Linked List node.
     */
    private class DLLNode {
        public final T data; // this field should never be updated. It gets its
                             // value once from the constructor DLLNode.
        public DLLNode next;
        public DLLNode prev;

        /**
         * Constructor
         * 
         * @param theData  : data of type T, to be stored in the node
         * @param prevNode : the previous Node in the Doubly Linked List
         * @param nextNode : the next Node in the Doubly Linked List
         * @return DLLNode
         */
        public DLLNode(T theData, DLLNode prevNode, DLLNode nextNode) {
            data = theData;
            prev = prevNode;
            next = nextNode;
        }
    }

    // Fields head and tail point to the first and last nodes of the list.
    private DLLNode head, tail;

    // length of list
    private int length = 0;

    /**
     * Constructor of an empty DLL
     * 
     * @return DoublyLinkedList
     */
    public DoublyLinkedList() {
        head = null;
        tail = null;
    }

    /**
     * Tests if the doubly linked list is empty
     * 
     * @return true if list is empty, and false otherwise
     *
     *         Worst-case asymptotic running time cost: Theta(1)
     *
     *         Justification: Only checks if one node is null.
     */
    public boolean isEmpty() {
        return head == null;
    }

    /**
     * Inserts an element in the doubly linked list
     * 
     * @param pos  : The integer location at which the new data should be inserted
     *             in the list. We assume that the first position in the list is 0
     *             (zero). If pos is less than 0 then add to the head of the list.
     *             If pos is greater or equal to the size of the list then add the
     *             element at the end of the list.
     * @param data : The new data of class T that needs to be added to the list
     * @return none
     *
     *         Worst-case asymptotic running time cost: Theta(n)
     *
     *         Justification: If the size is 0 or 1, or if the position is outside
     *         the list, the time taken is Theta(1) time. If the list's size is
     *         greater than 1 and pos is within the list, the while loop within the
     *         branch causes the method to take an additional Theta(n) time. In this
     *         worst case, this results in Theta(n) asymptotic time.
     */
    public void insertBefore(int pos, T data) {
        // checking for null data to fail quickly
        if (data == null) {
            return;
        }

        DLLNode node = new DLLNode(data, null, null);
        if (length == 0) { // size == 0
            head = node;
            tail = node;
        } else if (length == 1) { // size == 1
            if (pos <= 0) {
                node.next = head;
                head.prev = node;
                head = node;
            } else {
                node.prev = head;
                head.next = node;
                tail = node;
            }
        } else { // size > 1
            if (pos <= 0) {
                node.next = head;
                head.prev = node;
                head = node;
            } else if (pos > 0 && pos <= length - 1) {
                int i = 0;
                DLLNode node_a = new DLLNode(data, null, head); // dllnode before pos
                DLLNode node_b = new DLLNode(data, null, head); // dllnode after pos
                // set node_a to the node at position pos-1
                while (node != null) {
                    if (i < pos) {
                        node_a = node_a.next;
                    } else {
                        break;
                    }
                    i++;
                }

                // keep track of the replaced node
                node_b = node_a.next;

                // move new node in between node_a and node_b
                node_a.next = node;
                node.prev = node_a;
                node.next = node_b;
                node_b.prev = node;
            } else { // pos >= size
                node.prev = tail;
                tail.next = node;
                tail = node;
            }
        }
        setNewSize();
    }

    /**
     * Returns the data stored at a particular position
     * 
     * @param pos : the position
     * @return the data at pos, if pos is within the bounds of the list, and null
     *         otherwise.
     *
     *         Worst-case asymptotic running time cost: Theta(n)
     *
     *         Justification: If pos is at the end of the array, this method loops
     *         through the entire array. This takes Theta(n) time.
     *
     */
    public T get(int pos) {
        DLLNode node = head;
        int i = 0;
        while (node != null) {
            if (i == pos) {
                return node.data;
            }
            i++;
            node = node.next;
        }
        return null;
    }

    /**
     * Deletes the element of the list at position pos. First element in the list
     * has position 0. If pos points outside the elements of the list then no
     * modification happens to the list.
     * 
     * @param pos : the position to delete in the list.
     * @return true : on successful deletion, false : list has not been modified.
     *
     *         Worst-case asymptotic running time cost: Theta(n)
     *
     *         Justification: If pos is outside the array, the array contains 0 or 1
     *         element(s), or pos is either the head or tail, this method takes
     *         Theta(1) time. If pos is within the array, has more than 1 element,
     *         and is not the head or tail, the method loops through entire list
     *         once. This takes Theta(n) asymptotic time.
     */
    public boolean deleteAt(int pos) {
        DLLNode node = head; // node to be deleted
        if (pos < 0 || pos >= length) {
            // if outside, do nothing
            // an empty list is handled here, as pos = size = 0
            setNewSize();
            return false;
        } else if (head.next == null) {
            // branch for if the list has one node.
            head = null;
            setNewSize();
            return true;
        } else if (pos == 0) {
            // delete head
            head = head.next;
            setNewSize();
            head.prev = null;
            return true;
        } else if (pos == length - 1) {
            // delete tail
            tail = tail.prev;
            tail.next = null;
            setNewSize();
            return true;
        } else {
            int i = 0;
            while (node != null) {
                if (i == pos) {
                    // point node.prev and node.next around node to each other and set node to null
                    DLLNode p = node.prev;
                    DLLNode n = node.next;
                    p.next = n;
                    n.prev = p;
                    node = null;
                    break;
                }
                i++;
                node = node.next;
            }
            setNewSize();
            return true;
        }
    }

    /**
     * Reverses the list. If the list contains "A", "B", "C", "D" before the method
     * is called Then it should contain "D", "C", "B", "A" after it returns.
     *
     * Worst-case asymptotic running time cost: Theta(n)
     *
     * Justification: Pointer moves through list once. All other calls take
     * Theta(1), so n*Theta(1) = Theta(n) asymptotic time.
     */
    public void reverse() {
        // start at head. reverse the next and prev nodes using the aux node.

        DLLNode node = head; // pointer at start
        DLLNode aux = null; // auxiliary pointer

        if (length <= 1) {
            return;
        }

        // swap head and tail
        DLLNode temp = head;
        head = tail;
        tail = temp;

        // swap next and prev nodes for each node
        while (node != null) {
            aux = node.prev;
            node.prev = node.next;
            node.next = aux;
            node = node.prev;
        }

        // redefine head
        head = aux.prev;
    }

    /**
     * Removes all duplicate elements from the list. The method should remove the
     * _least_number_ of elements to make all elements uniqueue. If the list
     * contains "A", "B", "C", "B", "D", "A" before the method is called Then it
     * should contain "A", "B", "C", "D" after it returns. The relative order of
     * elements in the resulting list should be the same as the starting list.
     *
     * Worst-case asymptotic running time cost: Theta(n^2)
     *
     * Justification: For each node, the method loops over every other node. If p1
     * is head, the inner while loop runs n times, and we know that deleteAt() takes
     * Theta(n) time. In the worst case, this results in Theta(n*n) or Theta(n^2)
     * asymptotic time.
     */
    public void makeUnique() {
        // check if list is empty or has only 1 node.
        if (length <= 1) {
            return;
        }

        DLLNode p1, p2;
        int i = 1, ti = i; // i loops over p1, ti loops over p2
        for (p1 = head; p1 != null; p1 = p1.next) {
            p2 = p1.next;
            ti = i; // reset ti to i when i increases
            while (p2 != null) {
                if (p1.data == p2.data) {
                    // since p2 is going to be deleted, store p2.next in a temporary node.
                    // then set p2 to that temporary node.
                    // ti is not increase in this case as p2.next after p2 is deleted is still p2.
                    DLLNode temp = p2.next;
                    deleteAt(ti);
                    p2 = temp;
                } else {
                    p2 = p2.next;
                    ti++;
                }
            }
            i++;
        }
        setNewSize();
    }

    /*----------------------- STACK API 
     * If only the push and pop methods are called the data structure should behave like a stack.
     */

    /**
     * This method adds an element to the data structure. How exactly this will be
     * represented in the Doubly Linked List is up to the programmer.
     * 
     * @param item : the item to push on the stack
     *
     *             Worst-case asymptotic running time cost: Theta(n)
     *
     *             Justification: insertBefore() takes Theta(n) time. Assuming all
     *             other method calls take Theta(1) time, this results in Theta(n *
     *             1) or Theta(n) time.
     */
    public void push(T item) {
        // Add after tail.
        insertBefore(length, item);
    }

    /**
     * This method returns and removes the element that was most recently added by
     * the push method.
     * 
     * @return the last item inserted with a push; or null when the list is empty.
     *
     *         Worst-case asymptotic running time cost: Theta(n)
     *
     *         Justification: deleteAt() takes Theta(n) time. Assuming all other
     *         calls take Theta(1) time, this results in Theta(n) time.
     */
    public T pop() {
        // Return tail
        if (!isEmpty()) {
            T data = tail.data;
            deleteAt(length - 1);
            return data;
        }
        return null;

    }

    /*----------------------- QUEUE API
     * If only the enqueue and dequeue methods are called the data structure should behave like a FIFO queue.
     */

    /**
     * This method adds an element to the data structure. How exactly this will be
     * represented in the Doubly Linked List is up to the programmer.
     * 
     * @param item : the item to be enqueued to the stack
     *
     *             Worst-case asymptotic running time cost: Theta(n)
     *
     *             Justification: insertBefore() takes Theta(n) time. Assuming all
     *             other calls take Theta(1) time, this results in Theta(n) time.
     */
    public void enqueue(T item) {
        // simply add to tail
        insertBefore(length, item);
    }

    /**
     * This method returns and removes the element that was least recently added by
     * the enqueue method.
     * 
     * @return the earliest item inserted with an equeue; or null when the list is
     *         empty.
     *
     *         Worst-case asymptotic running time cost: Theta(n)
     *
     *         Justification: deleteAt() takes Theta(n) time. Assuming all other
     *         calls take Theta(1) time, this results in Theta(n) time.
     */
    public T dequeue() {
        // remove and return head
        if (!isEmpty()) {
            T item = head.data;
            deleteAt(0);
            return item;
        }
        return null;
    }

    /**
     * @return a string with the elements of the list as a comma-separated list,
     *         from beginning to end
     *
     *         Worst-case asymptotic running time cost: Theta(n)
     *
     *         Justification: We know from the Java documentation that
     *         StringBuilder's append() method runs in Theta(1) asymptotic time. We
     *         assume all other method calls here (e.g., the iterator methods above,
     *         and the toString method) will execute in Theta(1) time. Thus, every
     *         one iteration of the for-loop will have cost Theta(1). Suppose the
     *         doubly-linked list has 'n' elements. The for-loop will always iterate
     *         over all n elements of the list, and therefore the total cost of this
     *         method will be n*Theta(1) = Theta(n).
     */
    public String toString() {
        StringBuilder s = new StringBuilder();
        boolean isFirst = true;

        // iterate over the list, starting from the head
        for (DLLNode iter = head; iter != null; iter = iter.next) {
            if (!isFirst) {
                s.append(",");
            } else {
                isFirst = false;
            }
            s.append(iter.data.toString());
        }

        return s.toString();
    }

    /**
     *         Updates the field <code>length</code> to a new value.
     *
     *         Worst-case asymptotic running time cost: Theta(n)
     *
     *         Justification: Loops through entire array once, taking Theta(n) time.
     *         Assuming all normal method calls (setting head) take Theta(1) time,
     *         the result is n * Theta(1), or Theta(n) time.
     */
    private void setNewSize() {
        DLLNode n = head;
        int size = 0;
        while (n != null) {
            n = n.next;
            size++;
        }
        length = size;
    }

    /**
     * @return the size of the list as an integer.
     *
     *         Worst-case asymptotic running time cost: Theta(1)
     *
     *         Justification: Returns a single integer, which is done in constant time.
     */
    public int size() {
        return length;
    }

}

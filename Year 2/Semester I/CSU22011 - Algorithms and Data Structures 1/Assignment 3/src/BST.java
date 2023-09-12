
/*************************************************************************
 *  Binary Search Tree class.
 *  Adapted from Sedgewick and Wayne.
 *
 *  @version 3.0 1/11/15 16:49:42
 *
 *  @author Chike Okafor
 *
 *************************************************************************/

import java.util.NoSuchElementException;

public class BST<Key extends Comparable<Key>, Value> {
	private Node root; // root of BST

	/**
	 * Private node class.
	 */
	private class Node {
		private Key key; // sorted by key
		private Value val; // associated data
		private Node left, right; // left and right subtrees
		private int N; // number of nodes in subtree

		public Node(Key key, Value val, int N) {
			this.key = key;
			this.val = val;
			this.N = N;
		}
	}

	// is the symbol table empty?
	public boolean isEmpty() {
		return size() == 0;
	}

	// return number of key-value pairs in BST
	public int size() {
		return size(root);
	}

	// return number of key-value pairs in BST rooted at x
	private int size(Node x) {
		if (x == null)
			return 0;
		else
			return x.N;
	}

	/**
	 *  Search BST for given key.
	 *  Does there exist a key-value pair with given key?
	 *
	 *  @param key the search key
	 *  @return true if key is found and false otherwise
	 */
	public boolean contains(Key key) {
		return get(key) != null;
	}

	/**
	 *  Search BST for given key.
	 *  What is the value associated with given key?
	 *
	 *  @param key the search key
	 *  @return value associated with the given key if found, or null if no such key exists.
	 */
	public Value get(Key key) {
		return get(root, key);
	}

	private Value get(Node x, Key key) {
		if (x == null)
			return null;
		int cmp = key.compareTo(x.key);
		if (cmp < 0)
			return get(x.left, key);
		else if (cmp > 0)
			return get(x.right, key);
		else
			return x.val;
	}

	/**
	 *  Insert key-value pair into BST.
	 *  If key already exists, update with new value.
	 *
	 *  @param key the key to insert
	 *  @param val the value associated with key
	 */
	public void put(Key key, Value val) {
		if (val == null) {
			delete(key);
			return;
		}
		root = put(root, key, val);
	}

	private Node put(Node x, Key key, Value val) {
		if (x == null)
			return new Node(key, val, 1);
		int cmp = key.compareTo(x.key);
		if (cmp < 0)
			x.left = put(x.left, key, val);
		else if (cmp > 0)
			x.right = put(x.right, key, val);
		else
			x.val = val;
		x.N = 1 + size(x.left) + size(x.right);
		return x;
	}

	/**
	 * Tree height.
	 *
	 * Asymptotic worst-case running time using Theta notation: Theta(N) 
	 * The method recursively calls itself until it reaches a null node, and then it repeats,
	 * going over each subtree and therefore each node. For it to properly find the
	 * depth of the BST, it would have to recurse over each node once, so the
	 * running time is Theta(N)
	 *
	 * @return the number of links from the root to the deepest leaf.
	 *
	 * Example 1: for an empty tree this should return -1.
	 * Example 2: for a tree with only one node it should return 0.
	 * Example 3: for the following tree it should return 2.
	 *   B
	 *  / \
	 * A   C
	 *      \
	 *       D
	 */
	public int height() {
		return height(root) - 1;
	}

	private int height(Node x) {
		if (x == null) {
			return 0;
		}

		int lh = height(x.left) + 1;
		int rh = height(x.right) + 1;
		return (lh > rh ? lh : rh);
	}

	/**
	 * Median key. If the tree has N keys k1 < k2 < k3 < ... < kN, then their median
	 * key is the element at position (N+1)/2 (where "/" here is integer division)
	 *
	 * @return the median key, or null if the tree is empty.
	 */
	public Key median() {
		Node n = median(root, (size(root) - 1) / 2);
		return n == null ? null : n.key;
	}

	private Node median(Node x, int i) {
		// uses the select function given in the notes
		if (x == null) {
			return null;
		}
		int t = size(x.left);
		if (t > i) {
			return median(x.left, i);
		} else if (t < i) {
			return median(x.right, i - t - 1);
		} else {
			return x;
		}
	}

	/** 
	 * Print all keys of the tree in a sequence, in-order. That is, for each node,
	 * the keys in the left subtree should appear before the key in the node. Also,
	 * for each node, the keys in the right subtree should appear before the key in
	 * the node. For each subtree, its keys should appear within a parenthesis.
	 *
	 * Example 1: Empty tree -- output: "()".
	 * Example 2: Tree containing only "A" -- output: "(()A())" 
	 * Example 3: Tree: 
	 * 	 B 
	 * 	/ \ 
	 * A   C 
	 * 		\ 
	 * 		 D
	 *
	 * output: "((()A())B(()C(()D())))"
	 *
	 * output of example in the assignment:
	 * (((()A(()C()))E((()H(()M()))R()))S(()X()))
	 *
	 * @return a String with all keys in the tree, in order, parenthesized.
	 */
	public String printKeysInOrder() {
		return printKeysInOrder(root, "");
	}

	private String printKeysInOrder(Node x, String s) {
		StringBuilder sb = new StringBuilder(s);
		if (x == null) {
			sb.append("()");
		} else {
			sb.append("(");
			sb.append(printKeysInOrder(x.left, ""));
			sb.append(x.key);
			sb.append(printKeysInOrder(x.right, ""));
			sb.append(")");
		}
		return sb.toString();
	}

	/**
	 * Pretty Printing the tree. Each node is on one line -- see assignment for
	 * details.
	 *
	 * @return a multi-line string with the pretty ascii picture of the tree.
	 */
	public String prettyPrintKeys() {
		return prettyPrint(root, "");
	}

	private String prettyPrint(Node node, String prefix) {
		StringBuilder sb = new StringBuilder(prefix);
		if (node != null) {
			prefix += " |"; // add " |" prefix for each new line
			sb.append("-" + node.key + "\n"); // add "-" before each key
			sb.append(prettyPrint(node.left, prefix)); // traverse down the left side of the BST using the current prefix
			// traverse down the right side of the BST and replace the final character of the prefix with a space
			sb.append(prettyPrint(node.right, prefix.substring(0, prefix.length() - 1) + " "));
		} else {
			sb.append("-null\n");
		}

		return sb.toString();
	}

	/**
	 * Deteles a key from a tree (if the key is in the tree). Note that this method
	 * works symmetrically from the Hibbard deletion: If the node to be deleted has
	 * two child nodes, then it needs to be replaced with its predecessor (not its
	 * successor) node.
	 *
	 * @param key the key to delete
	 */

	// ! Note: predecessor: largest node in left subtree, successor: smallest node
	// ! in right subtree
	public void delete(Key key) {
		root = delete(root, key);
	}

	private Node delete(Node x, Key key) {
		if (x == null) {
			return null;
		}
		int cmp = key.compareTo(x.key);
		if (cmp < 0) {
			x.left = delete(x.left, key);
		} else if (cmp > 0) {
			x.right = delete(x.right, key);
		} else {
			if (x.right == null) {
				return x.left;
			}
			if (x.left == null) {
				return x.right;
			}

			Node t = x;
			x = maxNode(t.left);
			x.left = deleteMax(t.left);
			x.right = t.right;

		}
		x.N = size(x.left) + size(x.right) + 1;
		return x;
	}

	/**
	 * Deletes the mamximum key.
	 * 
	 * @param key the key to delete
	 */
	public void deleteMax() {
		root = deleteMax(root);
	}

	private Node deleteMax(Node x) {
		if (x == null) {
			return null;
		}

		if (x.right == null) {
			return x.left;
		}
		x.right = deleteMax(x.right);
		x.N = 1 + size(x.left) + size(x.right);
		return x;
	}

	private Node maxNode(Node x) {
		while (x.right != null) {
			x = x.right;
		}
		return x;
	}

	/**
	 * Returns BST as an in-order string.
	 */
	public String toString() {
		// multiply size() by n since for each key, there is an additional n-1
		// characters (in this case ", ")
		// subtract n-1 to remove final characters
		if (!isEmpty()) {
			String string = toString(root, "");
			return "[" + string.substring(0, string.length()-2) + "]";
		}
		return "";
	}

	private String toString(Node x, String s) {
		StringBuilder sb = new StringBuilder(s);
		if (x != null) {
			sb.append(toString(x.left, ""));
			sb.append("\"" + x.key + "\"" + ", ");
			sb.append(toString(x.right, ""));
		}
		return sb.toString();
	}
}

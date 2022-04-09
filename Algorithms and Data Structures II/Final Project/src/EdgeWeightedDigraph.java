/**
 * This is a class adapted from the file found here: https://algs4.cs.princeton.edu/44sp/EdgeWeightedDigraph.java.html
 * The code written here was originally created by Robert Sedgewick and Kevin Wayne, but I have adapted it
 * to suit my own needs.
 * @author: Chike Okafor
 */


import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class EdgeWeightedDigraph {
    private final int V;
    private int E;
    private final ArrayList<DirectedEdge>[] adj;
    private final List<Integer> nodes;

    // max is the highest value within the arraylist (not necessarily the last)
    EdgeWeightedDigraph(ArrayList<Integer> nds, int max) {
        this.V = nds.size();
        this.E = 0;
        this.nodes = nds;
        adj = (ArrayList<DirectedEdge>[]) new ArrayList[max + 1];
        for (int i : nds) {
            adj[i] = new ArrayList<>();
        }
    }

    public int E() {
        return E;
    }

    public int V() {
        return V;
    }

    public void addEdge(DirectedEdge e) {
        int v = e.from();
        adj[v].add(e);
        E++;
    }

    public List<Integer> nodes() {
        return nodes;
    }

    public Iterable<DirectedEdge> adj(int v) {
        return adj[v];
    }

    public ArrayList<DirectedEdge> edges() {
        ArrayList<DirectedEdge> list = new ArrayList<>();
        
        for (int v : nodes) {
            for (DirectedEdge e : adj(v)) {
                list.add(e);
            }
        }
        return list;
    }

    // remove duplicate edges (where de.to(), de.from(), and de.weight() are equal values within another edge)
    public void purge() {
        for (int v : nodes) {
            ArrayList<DirectedEdge> arr = new ArrayList<>(); // create a new arraylist
            adj(v).forEach(arr :: add); // add each value from the iterable adj(v) to the arraylist
            Set<DirectedEdge> s = new HashSet<>(arr); // create a new hashset from the arraylist
            adj[v] = new ArrayList<>(); // clear adj[v]
            adj[v].addAll(s); // replace the value in adj[v] with the new hashset arraylist
        }
    }
}

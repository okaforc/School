
/**
 * This is a class adapted from the file found here: https://algs4.cs.princeton.edu/44sp/EdgeWeightedDigraph.java.html
 * The code written here was originally created by Robert Sedgewick and Kevin Wayne, but I have adapted it
 * to suit my own needs.
 * @author: Chike Okafor
 */


import java.util.*;

public class EdgeWeightedDigraph {
    private int V, E;
    private ArrayList<DirectedEdge>[] adj;
    private List<Integer> nodes;
    // private int[] indegree;

    EdgeWeightedDigraph(int V) {
        this.V = V;
        this.E = 0;
        adj = (ArrayList<DirectedEdge>[]) new ArrayList[V];
        for (int i = 0; i < V; i++) {
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

    public void setNodes(List<Integer> nodes) {
        this.nodes = nodes;
    }

    public Iterable<DirectedEdge> adj(int v) {
        return adj[v];
    }

    public Iterable<DirectedEdge> edges() {
        List<DirectedEdge> list = new ArrayList<>();
        for (int v = 0; v < V; v++) {
            for (DirectedEdge e : adj(v)) {
                list.add(e);
            }
        }
        return list;
    }
}


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

    public void setNodes(List<Integer> nodes) {
        this.nodes = nodes;
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
}

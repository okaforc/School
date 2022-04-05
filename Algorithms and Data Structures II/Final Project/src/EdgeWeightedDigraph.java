
/**
 * @author: mmmmmmm so scrumptions
 */


import java.util.*;

public class EdgeWeightedDigraph {
    private int V, E;
    private ArrayList<DirectedEdge>[] adj;
    private List<Integer> nodes;
    private List<Integer> edges;
    // private int[] indegree;

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

    // remove duplicate edges (where de.to(), de.from(), and de.weight() are equal values within another edge)
    public void purge() {
        for (int v : nodes) {
            Set<DirectedEdge> s = new HashSet<>(); // create a new empty hashset
            ArrayList<DirectedEdge> arr = new ArrayList<>(); // create a new arraylist
            adj(v).forEach(arr :: add); // add each value from adj(v) to the arraylist
            s.addAll(arr); // add each value in the arraylist to the hashset
            adj[v] = new ArrayList<>(); // clear adj[v]
            adj[v].addAll(s); // replace the value in adj[v] with the new hashset arraylist
        }
    }
}

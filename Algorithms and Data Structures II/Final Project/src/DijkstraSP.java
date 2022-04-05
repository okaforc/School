
/**
 * @author: my big juicy ass
 */

import java.util.*;

public class DijkstraSP {
    private double[] distTo;
    private DirectedEdge[] edgeTo; // edgeTo[v] = last edge on shortest s->v path

    DijkstraSP(EdgeWeightedDigraph G, int s, int max) {
        distTo = new double[max + 1];
        edgeTo = new DirectedEdge[max + 1];

        for (int i = 0; i < max + 1; i++) {
            distTo[i] = Double.POSITIVE_INFINITY;
        }

        distTo[s] = 0.0;

        fullRelax(G);
    }

    public void fullRelax(EdgeWeightedDigraph G) {
        Integer[] tnodes = G.nodes().toArray(new Integer[G.V()]);
        Arrays.sort(tnodes);

        while (!check(G)) {
            for (int i : tnodes) {
                for (DirectedEdge de : G.adj(i)) {
                    relax(de);
                }
            }
        }
    }

    public double distTo(int v) {
        return distTo[v];
    }

    public boolean hasPathTo(int v) {
        try {
            return distTo[v] < Double.POSITIVE_INFINITY;
        } catch (Exception e) {
            return false;
        }

    }

    public void relax(DirectedEdge e) {
        int v = e.from(), w = e.to();
        if (distTo[w] > distTo[v] + e.weight()) {
            distTo[w] = distTo[v] + e.weight();
            edgeTo[w] = e;
            // BigDecimal bd1 = new BigDecimal(Double.toString(distTo(v)));
            // BigDecimal bd2 = new BigDecimal(Double.toString(e.weight()));
            // distTo[w] = Double.parseDouble(bd1.add(bd2).toString());

        }
    }

    /**
     * Returns a shortest path from the source vertex {@code s} to vertex {@code v}.
     *
     * @param  v the destination vertex
     * @return a shortest path from the source vertex {@code s} to vertex {@code v}
     *         as an iterable of edges, and {@code null} if no such path
     * @throws IllegalArgumentException unless {@code 0 <= v < V}
     */
    public ArrayList<DirectedEdge> pathTo(int v) {
        if (!hasPathTo(v))
            return null;
        Stack<DirectedEdge> path = new Stack<DirectedEdge>();
        for (DirectedEdge e = edgeTo[v]; e != null; e = edgeTo[e.from()]) {
            path.push(e);
        }
        ArrayList<DirectedEdge> arr = FileHandler.arrayToList(path);
        Collections.reverse(arr);
        return arr;
    }

    public boolean check(EdgeWeightedDigraph G) {
        // check that all edges e = v->w satisfy distTo[w] <= distTo[v] + e.weight()
        for (int v : G.nodes()) {
            for (DirectedEdge e : G.adj(v)) {
                int w = e.to();
                if (distTo[v] + e.weight() < distTo[w]) {
                    // System.err.println("edge " + e + " not relaxed");
                    return false;
                }
            }
        }
        return true;
    }
}
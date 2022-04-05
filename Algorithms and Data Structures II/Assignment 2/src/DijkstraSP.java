/**
 * This is a class adapted from the file found here: https://algs4.cs.princeton.edu/44sp/DijkstraSP.java.html
 * The code written here was originally created by Robert Sedgewick and Kevin Wayne, but I have adapted it
 * to suit my own needs.
 * @author: Chike Okafor
 */

import java.util.*;

public class DijkstraSP {
    private double[] distTo;

    DijkstraSP(EdgeWeightedDigraph G, int s) {
        distTo = new double[G.V()];

        for (int i = 0; i < G.V(); i++) {
            distTo[i] = Double.POSITIVE_INFINITY;
        }

        distTo[s] = 0.0;

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
            // BigDecimal bd1 = new BigDecimal(Double.toString(distTo(v)));
            // BigDecimal bd2 = new BigDecimal(Double.toString(e.weight()));
            // distTo[w] = Double.parseDouble(bd1.add(bd2).toString());
            
        }
    }

    public boolean check(EdgeWeightedDigraph G) {
        // check that all edges e = v->w satisfy distTo[w] <= distTo[v] + e.weight()
        for (int v = 0; v < G.V(); v++) {
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
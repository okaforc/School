/**
 * This is a class adapted from the file found here: https://algs4.cs.princeton.edu/44sp/DirectedEdge.java.html
 * The code written here was originally created by Robert Sedgewick and Kevin Wayne, but I have adapted it
 * to suit my own needs.
 * @author Chike Okafor
 */

public class DirectedEdge {
    private double weight;
    private int v, w;

    DirectedEdge(int v, int w, double weight) {
        this.v = v;
        this.w = w;
        this.weight = weight;
    }

    public double weight() {
        return weight;
    }

    public int from() {
        return v;
    }

    public int to() {
        return w;
    }

    public String toString() {
        return v + "->" + w + " " + String.format("%5.2f", weight);
    }

}

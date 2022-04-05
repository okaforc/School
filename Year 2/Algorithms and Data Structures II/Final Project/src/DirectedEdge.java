/**
 * This is a class adapted from the file found here: https://algs4.cs.princeton.edu/44sp/DirectedEdge.java.html
 * The code written here was originally created by Robert Sedgewick and Kevin Wayne, but I have adapted it
 * to suit my own needs.
 * @author Chike Okafor
 */

public class DirectedEdge {
    private final double weight;
    private final int v, w;

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

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + v;
        result = prime * result + w;
        long temp;
        temp = Double.doubleToLongBits(weight);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        return result;
    }

    // checks if two DirectedEdges are the same
    @Override
    public boolean equals(Object de) {
        if (de instanceof DirectedEdge temp) {
            return (this.v == temp.from() && this.w == temp.to() && this.weight == temp.weight());
        }
        return false;
    }
}

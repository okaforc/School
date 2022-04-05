import java.util.*;
import java.io.*;
import java.math.BigDecimal;

/**
 * A Contest to Meet (ACM) is a reality TV contest that sets three contestants at three random
 * city intersections. In order to win, the three contestants need all to meet at any intersection
 * of the city as fast as possible.
 * It should be clear that the contestants may arrive at the intersections at different times, in
 * which case, the first to arrive can wait until the others arrive.
 * From an estimated walking speed for each one of the three contestants, ACM wants to determine the
 * minimum time that a live TV broadcast should last to cover their journey regardless of the contestants’
 * initial positions and the intersection they finally meet. You are hired to help ACM answer this question.
 * You may assume the following:
 *     Each contestant walks at a given estimated speed.
 *     The city is a collection of intersections in which some pairs are connected by one-way
 * streets that the contestants can use to traverse the city.
 *
 * This class implements the competition using Floyd-Warshall algorithm
 * 
 * @author Chike Okafor
 */

public class CompetitionFloydWarshall {

    private int sa;
    private int sb;
    private int sc;
    public List<Integer> nodes; // List of all nodes in the graph
    public EdgeWeightedDigraph ewd; // the graph being worked on from the file being used
    public boolean isValidGraph = true; // boolean to check if the walking speeds are valid

    /**
     * @param filename: A filename containing the details of the city road network
     * @param sA, sB, sC: speeds for 3 contestants
     */
    CompetitionFloydWarshall(String filename, int sA, int sB, int sC) {
        sa = sA;
        sb = sB;
        sc = sC;
        if (initGraph(filename) == -1 || ((sa < 50 || sa > 100) || (sb < 50 || sb > 100) || (sc < 50 || sc > 100))) {
            isValidGraph = false;
        }
    }

    public int initGraph(String filename) {
        try {
            BufferedReader br = new BufferedReader(new FileReader(filename));
            // speeds of the players, and the number of nodes in the graph
            int numNodes = Integer.parseInt(br.readLine()); // get the number of nodes in the graph (line 1)
            br.readLine(); // skip the number of edges in the graph and advance onto the next line
            ewd = new EdgeWeightedDigraph(numNodes);
            nodes = new ArrayList<>(numNodes);
            String line = br.readLine();

            while (line != null) {
                String[] vals = line.trim().split("\\p{Z}+");
                int n1 = Integer.parseInt(vals[0]); // get the starting vertex v
                int n2 = Integer.parseInt(vals[1]); // get the destination vertex w
                double nodeDist = Double.parseDouble(vals[2]); // get the edge weight

                if (!nodes.contains(n1)) {
                    nodes.add(n1);
                }

                if (!nodes.contains(n2)) {
                    nodes.add(n2);
                }

                ewd.addEdge(new DirectedEdge(n1, n2, nodeDist)); // add this new edge to the ArrayList
                line = br.readLine(); // move onto the next line
            }
            ewd.setNodes(nodes); // set the graph's nodes as the ArrayList nodes
            br.close();
        } catch (Exception e) {
            //e.printStackTrace();
            return -1; // Return -1 if file name/data is invalid
        }
        return 0;
    }

    /**
     * @return int: minimum minutes that will pass before the three contestants can meet
     */
    public int timeRequiredforCompetition() {
        if (!isValidGraph) return -1;
        double[][] matrix = floydWarshall(ewd);
        double furthest = -1;
        List<Integer> speeds = new ArrayList<>();
        speeds.add(sa);
        speeds.add(sb);
        speeds.add(sc);
        int slowest = Collections.min(speeds);
        for (double[] ds : matrix) {
            for (double d : ds) {
                if (d > furthest) {
                    furthest = d;
                }
            }
        }
        
        int maxTime = (int) Math.ceil((furthest * 1000) / slowest);
        return (maxTime == Integer.MAX_VALUE) || (maxTime < 0) ? -1 : maxTime;
    }

    /**
     * 
     * @param G The graph being used.
     * @return An adjacency matrix as a double[][] of the shortest paths to all other nodes from a start node using the Floyd-Warshall algorithm.
     */
    public double[][] floydWarshall(EdgeWeightedDigraph G) {
        double[][] distance;
        int n = G.V();
        distance = new double[n][n];

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                distance[i][j] = Double.POSITIVE_INFINITY; // initialise every value as INF
            }
            distance[i][i] = 0; // set the diagonals of the adjacency matrix as 0
        }

        // add each edge into the matrix
        for (DirectedEdge edge : G.edges()) {
            distance[edge.from()][edge.to()] = edge.weight(); 
        }
        
        // set the distance of the shortest path for every node pair.
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                for (int k = 0; k < n; k++) {
                    if (distance[j][k] > distance[j][i] + distance[i][k]) {
                        BigDecimal bd1 = new BigDecimal(Double.toString(distance[j][i]));
                        BigDecimal bd2 = new BigDecimal(Double.toString(distance[i][k]));
                        distance[j][k] = bd1.add(bd2).doubleValue();
                    }
                }
            }
        }

        return distance;
    }


}


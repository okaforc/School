
/**
 * @author Chike Okafor
 */

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.stream.*;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import static org.junit.Assert.*;

public class CompetitionTests {

    @Test
    public void testDijkstraConstructor() {
        CompetitionDijkstra dj;
        String filename = "tinyEWD.txt";

        dj = new CompetitionDijkstra(filename, 50, 50, 50);
        assertTrue("Expecting a valid graph.", dj.isValidGraph);
        dj = new CompetitionDijkstra(filename, 10, 50, 100);
        assertFalse("Expecting an invalid graph.", dj.isValidGraph);

        filename = "fakeFile.txt";
        dj = new CompetitionDijkstra(filename, 50, 50, 50);
        assertFalse("Expecting an invalid graph.", dj.isValidGraph);
    }

    @Test
    public void testFWConstructor() {
        CompetitionFloydWarshall fw;
        String filename = "tinyEWD.txt";

        fw = new CompetitionFloydWarshall(filename, 50, 50, 50);
        assertTrue("Expecting a valid graph.", fw.isValidGraph);
        fw = new CompetitionFloydWarshall(filename, 10, 50, 100);
        assertFalse("Expecting an invalid graph.", fw.isValidGraph);

        filename = "fakeFile.txt";
        fw = new CompetitionFloydWarshall(filename, 50, 50, 50);
        assertFalse("Expecting an invalid graph.", fw.isValidGraph);
    }

    @Test
    public void testInitGraph() {
        CompetitionDijkstra dj;
        String filename = "tinyEWD.txt";

        dj = new CompetitionDijkstra(filename, 50, 50, 50);
        assertEquals(0, dj.initGraph(filename));

        dj = new CompetitionDijkstra(filename, 50, 500, 50);
        assertEquals(0, dj.initGraph(filename));

        filename = "fakeFile.txt";
        dj = new CompetitionDijkstra(filename, 50, 50, 50);
        assertEquals(-1, dj.initGraph(filename));

        dj = new CompetitionDijkstra(filename, 10, 20, 30);
        assertEquals(-1, dj.initGraph(filename));

        filename = "1000EWD.txt";
        dj = new CompetitionDijkstra(filename, 100, 100, 100);
        assertEquals(0, dj.initGraph(filename));

        dj = new CompetitionDijkstra(filename, 100, 100, 100);
        assertEquals(0, dj.initGraph(filename));

        CompetitionFloydWarshall fw;
        filename = "tinyEWD.txt";

        fw = new CompetitionFloydWarshall(filename, 50, 50, 50);
        assertEquals(0, fw.initGraph(filename));

        fw = new CompetitionFloydWarshall(filename, 50, 500, 50);
        assertEquals(0, fw.initGraph(filename));

        filename = "fakeFile.txt";
        fw = new CompetitionFloydWarshall(filename, 50, 50, 50);
        assertEquals(-1, fw.initGraph(filename));

        fw = new CompetitionFloydWarshall(filename, 10, 20, 30);
        assertEquals(-1, fw.initGraph(filename));

        filename = "1000EWD.txt";
        fw = new CompetitionFloydWarshall(filename, 100, 100, 100);
        assertEquals(0, fw.initGraph(filename));

        fw = new CompetitionFloydWarshall(filename, 100, 100, 100);
        assertEquals(0, fw.initGraph(filename));
    }

    @Test
    public void testDijkstra() {
        CompetitionDijkstra dj;
        String filename = "tinyEWD.txt";

        dj = new CompetitionDijkstra(filename, 50, 50, 50);

        assertEquals("[1.4500000000000002, 1.54, 1.26, 1.86, 1.1, 0.92, 0.0, 0.52]",
                Arrays.toString(dj.dijkstra(3).toArray()));
        assertEquals("[0.38, 0.73, 0.6000000000000001, 1.05, 0.0, 0.26, 0.9900000000000001, 1.5100000000000002]",
                Arrays.toString(dj.dijkstra(0).toArray()));
        assertNull(dj.dijkstra(10));

        dj = new CompetitionDijkstra(filename, 50, 50, 500);
        assertNull(dj.dijkstra(4));

        dj = new CompetitionDijkstra(filename, 50, 63, 77);
        assertNull(dj.dijkstra(123));

        dj = new CompetitionDijkstra(filename, 50, 63, 77);
        assertEquals("[0.93, 1.02, 0.74, 1.34, 0.58, 0.4, 1.13, 0.0]", Arrays.toString(dj.dijkstra(6).toArray()));
    }

    @Test
    public void testFloydWarshall() {
        CompetitionFloydWarshall fw;
        String filename = "tinyEWD.txt";

        fw = new CompetitionFloydWarshall(filename, 50, 50, 50);

        double[][] s = { { 0.0, 1.05, 0.26, 0.99, 0.38, 0.73, 1.51, 0.6 },
                { 1.39, 0.0, 1.21, 0.29, 1.74, 1.83, 0.81, 1.55 }, { 1.83, 0.94, 0.0, 0.73, 0.97, 0.62, 1.25, 0.34 },
                { 1.1, 1.86, 0.92, 0.0, 1.45, 1.54, 0.52, 1.26 }, { 1.86, 0.67, 1.68, 0.76, 0.0, 0.35, 1.28, 0.37 },
                { 1.71, 0.32, 1.53, 0.61, 0.35, 0.0, 1.13, 0.28 }, { 0.58, 1.34, 0.4, 1.13, 0.93, 1.02, 0.0, 0.74 },
                { 1.49, 0.6, 1.31, 0.39, 0.63, 0.28, 0.91, 0.0 } };

        assertArrayEquals(s, fw.floydWarshall(fw.ewd));
    }

    @Test
    public void testTimeRequiredforCompetition() {
        // Dijkstra

        CompetitionDijkstra dj;
        String filename = "tinyEWD.txt";

        dj = new CompetitionDijkstra(filename, 50, 50, 50);
        assertEquals(38, dj.timeRequiredforCompetition());

        dj = new CompetitionDijkstra(filename, 50, 500, 50);
        assertEquals(-1, dj.timeRequiredforCompetition());

        filename = "fakeFile.txt";
        dj = new CompetitionDijkstra(filename, 50, 50, 50);
        assertEquals(-1, dj.timeRequiredforCompetition());

        dj = new CompetitionDijkstra(filename, 10, 20, 30);
        assertEquals(-1, dj.timeRequiredforCompetition());

        filename = "tinyEWD.txt";
        dj = new CompetitionDijkstra(filename, 100, 100, 100);
        assertEquals(19, dj.timeRequiredforCompetition());

        filename = "1000EWD.txt";
        dj = new CompetitionDijkstra(filename, 100, 100, 100);
        assertEquals(14, dj.timeRequiredforCompetition());

        // Floyd-Warshall

        filename = "tinyEWD.txt";

        CompetitionFloydWarshall fw = new CompetitionFloydWarshall(filename, 50, 50, 50);
        assertEquals(38, fw.timeRequiredforCompetition());

        fw = new CompetitionFloydWarshall(filename, 50, 500, 50);
        assertEquals(-1, fw.timeRequiredforCompetition());

        filename = "fakeFile.txt";
        fw = new CompetitionFloydWarshall(filename, 50, 50, 50);
        assertEquals(-1, fw.timeRequiredforCompetition());

        fw = new CompetitionFloydWarshall(filename, 10, 20, 30);
        assertEquals(-1, fw.timeRequiredforCompetition());

        filename = "tinyEWD.txt";
        fw = new CompetitionFloydWarshall(filename, 100, 100, 100);
        assertEquals(19, fw.timeRequiredforCompetition());

        fw = new CompetitionFloydWarshall(filename, 50, 69, 69);
        assertEquals(38, fw.timeRequiredforCompetition());
    }

    @Test
    public void testDijkstraSPConstructor() {
        CompetitionDijkstra dj;
        String filename = "tinyEWD.txt";

        dj = new CompetitionDijkstra(filename, 50, 50, 50);

        int source = 4;
        DijkstraSP sp = new DijkstraSP(dj.ewd, source);

        assertEquals(0.35, sp.distTo(5), 0);
    }

    @Test
    public void testDistTo() {
        CompetitionDijkstra dj;
        String filename = "tinyEWD.txt";

        dj = new CompetitionDijkstra(filename, 50, 50, 50);

        int source = 4;
        DijkstraSP sp = new DijkstraSP(dj.ewd, source);

        assertEquals(0.35, sp.distTo(5), 0.000000001);
        assertEquals(1.28, sp.distTo(6), 0.000000001);
        assertEquals(0.0, sp.distTo(4), 0.000000001);
        assertEquals(0.67, sp.distTo(1), 0.000000001);

        filename = "1000EWD.txt";
        source = 123;
        dj = new CompetitionDijkstra(filename, 50, 50, 50);
        sp = new DijkstraSP(dj.ewd, source);
        assertEquals(0.50414, sp.distTo(992), 0.000000001);
        assertEquals(0.0, sp.distTo(123), 0.000000001);

        source = 913;
        sp = new DijkstraSP(dj.ewd, source);
        assertEquals(0.11058, sp.distTo(432), 0.000000001);
        assertEquals(0.81627, sp.distTo(33), 0.000000001);
        assertEquals(0.53827, sp.distTo(2), 0.000000001);
    }

    @Test
    public void testHasPathTo() {
        CompetitionDijkstra dj;
        String filename = "tinyEWD.txt";

        dj = new CompetitionDijkstra(filename, 50, 50, 50);

        int source = 4;
        DijkstraSP sp = new DijkstraSP(dj.ewd, source);

        assertTrue("Expecting a path", sp.hasPathTo(5));
        assertTrue("Expecting a path", sp.hasPathTo(6));
        assertTrue("Expecting a path", sp.hasPathTo(4));
        assertTrue("Expecting a path", sp.hasPathTo(1));

        filename = "1000EWD.txt";
        source = 123;
        dj = new CompetitionDijkstra(filename, 50, 50, 50);
        sp = new DijkstraSP(dj.ewd, source);
        assertTrue("Expecting a path", sp.hasPathTo(992));
        assertTrue("Expecting a path", sp.hasPathTo(123));

        source = 913;
        sp = new DijkstraSP(dj.ewd, source);
        assertTrue("Expecting a path", sp.hasPathTo(432));
        assertTrue("Expecting a path", sp.hasPathTo(33));
        assertFalse("Expecting no path", sp.hasPathTo(-2));
        assertFalse("Expecting no path", sp.hasPathTo((int) Double.POSITIVE_INFINITY));
    }

    @Test
    public void testFrom() {
        DirectedEdge de = new DirectedEdge(1, 3, 34);
        assertEquals(1, de.from());
        de = new DirectedEdge(-132, 23209424, -22.22222);
        assertEquals(-132, de.from());
        de = new DirectedEdge(Integer.MAX_VALUE, 23209424, -22.22222);
        assertEquals(2147483647, de.from());
        de = new DirectedEdge(Integer.MIN_VALUE, 23209424, -22.22222);
        assertEquals(-2147483648, de.from());
    }

    @Test
    public void testTo() {
        DirectedEdge de = new DirectedEdge(1, 3, 34);
        assertEquals(3, de.to());
        de = new DirectedEdge(-132, -23209424, -22.22222);
        assertEquals(-23209424, de.to());
        de = new DirectedEdge(Integer.MAX_VALUE, Integer.MAX_VALUE, -22.2222312);
        assertEquals(2147483647, de.to());
        de = new DirectedEdge(Integer.MIN_VALUE, Integer.MIN_VALUE, 9323.22222);
        assertEquals(-2147483648, de.to());
    }

    @Test
    public void testWeight() {
        DirectedEdge de = new DirectedEdge(1, 3, 34);
        assertEquals(34, de.weight(), 0);
        de = new DirectedEdge(-132, -23209424, -22.22222);
        assertEquals(-22.22222, de.weight(), 0);
        de = new DirectedEdge(Integer.MAX_VALUE, Integer.MAX_VALUE, -22.2222312);
        assertEquals(-22.2222312, de.weight(), 0);
        de = new DirectedEdge(Integer.MIN_VALUE, Integer.MIN_VALUE, 9323.11111222323242);
        assertEquals(9323.11111222323242, de.weight(), 0);
    }

    @Test
    public void testE() {
        CompetitionDijkstra dj;
        String filename = "tinyEWD.txt";

        dj = new CompetitionDijkstra(filename, 50, 50, 50);
        assertEquals(15, dj.ewd.E());

        filename = "1000EWD.txt";
        dj = new CompetitionDijkstra(filename, 50, 50, 50);
        assertEquals(16866, dj.ewd.E());
    }

    @Test
    public void testV() {
        CompetitionDijkstra dj;
        String filename = "tinyEWD.txt";

        dj = new CompetitionDijkstra(filename, 50, 50, 50);
        assertEquals(8, dj.ewd.V());

        filename = "1000EWD.txt";
        dj = new CompetitionDijkstra(filename, 50, 50, 50);
        assertEquals(1000, dj.ewd.V());
    }

    @Test
    public void testAddEdge() {
        // Convert Iterable to List

        CompetitionDijkstra cd = new CompetitionDijkstra("tinyEWD.txt", 50, 50, 50);
        EdgeWeightedDigraph ewd = cd.ewd;

        // Get an Iterable object as an ArrayList, then get that as a String.
        String graphString = iterString(ewd.edges());
        assertEquals(
                "[0->4  0.38, 0->2  0.26, 1->3  0.29, 2->7  0.34, 3->6  0.52, 4->5  0.35, 4->7  0.37, 5->4  0.35, 5->7  0.28, 5->1  0.32, 6->2  0.40, 6->0  0.58, 6->4  0.93, 7->5  0.28, 7->3  0.39]",
                graphString);

        ewd.addEdge(new DirectedEdge(1, 7, 0.35));
        graphString = iterString(ewd.edges());
        assertEquals(
                "[0->4  0.38, 0->2  0.26, 1->3  0.29, 1->7  0.35, 2->7  0.34, 3->6  0.52, 4->5  0.35, 4->7  0.37, 5->4  0.35, 5->7  0.28, 5->1  0.32, 6->2  0.40, 6->0  0.58, 6->4  0.93, 7->5  0.28, 7->3  0.39]",
                graphString);

        ewd.addEdge(new DirectedEdge(2, 6, 0.75));
        graphString = iterString(ewd.edges());

        assertEquals(
                "[0->4  0.38, 0->2  0.26, 1->3  0.29, 1->7  0.35, 2->7  0.34, 2->6  0.75, 3->6  0.52, 4->5  0.35, 4->7  0.37, 5->4  0.35, 5->7  0.28, 5->1  0.32, 6->2  0.40, 6->0  0.58, 6->4  0.93, 7->5  0.28, 7->3  0.39]",
                graphString);
    }

    @Test
    public void testAdj() {
        CompetitionDijkstra cd = new CompetitionDijkstra("tinyEWD.txt", 50, 50, 50);
        EdgeWeightedDigraph ewd = cd.ewd;

        String graphString = iterString(ewd.adj(0));
        assertEquals("[0->4  0.38, 0->2  0.26]", graphString);

        graphString = iterString(ewd.adj(5));
        assertEquals("[5->4  0.35, 5->7  0.28, 5->1  0.32]", graphString);

        CompetitionFloydWarshall fw = new CompetitionFloydWarshall("1000EWD.txt", 50, 50, 50);
        ewd = fw.ewd;

        graphString = iterString(ewd.adj(4));
        assertEquals(
                "[4->76  0.05, 4->140  0.07, 4->150  0.02, 4->193  0.07, 4->199  0.06, 4->318  0.05, 4->323  0.06, 4->330  0.03, 4->356  0.07, 4->434  0.06, 4->569  0.07, 4->653  0.04, 4->658  0.07, 4->822  0.06, 4->864  0.07, 4->879  0.02, 4->941  0.05]",
                graphString);

        graphString = iterString(ewd.adj(400));
        assertEquals(
                "[400->441  0.07, 400->510  0.06, 400->529  0.01, 400->548  0.07, 400->557  0.07, 400->592  0.05, 400->726  0.07, 400->769  0.04, 400->880  0.03, 400->345  0.01, 400->245  0.06, 400->223  0.07, 400->179  0.03, 400->67  0.02, 400->35  0.06]",
                graphString);
    }

    @Test
    public void testEdges() {
        // Convert Iterable to List

        // System.out.println(Arrays.toString(cd.dijkstra(6).toArray()));
        CompetitionDijkstra cd = new CompetitionDijkstra("tinyEWD.txt", 50, 50, 50);
        String edges = iterString(cd.ewd.edges());
        assertEquals(
                "[0->4  0.38, 0->2  0.26, 1->3  0.29, 2->7  0.34, 3->6  0.52, 4->5  0.35, 4->7  0.37, 5->4  0.35, 5->7  0.28, 5->1  0.32, 6->2  0.40, 6->0  0.58, 6->4  0.93, 7->5  0.28, 7->3  0.39]",
                edges);

        CompetitionFloydWarshall fw = new CompetitionFloydWarshall("tinyEWD.txt", 50, 50, 50);
        edges = iterString(fw.ewd.edges());
        assertEquals(
                "[0->4  0.38, 0->2  0.26, 1->3  0.29, 2->7  0.34, 3->6  0.52, 4->5  0.35, 4->7  0.37, 5->4  0.35, 5->7  0.28, 5->1  0.32, 6->2  0.40, 6->0  0.58, 6->4  0.93, 7->5  0.28, 7->3  0.39]",
                edges);
    }

    @Test
    public void testNodes() {
        CompetitionDijkstra cd = new CompetitionDijkstra("tinyEWD.txt", 60, 70, 80);
        String nodes = Arrays.toString(cd.ewd.nodes().toArray());
        assertEquals("[4, 5, 7, 1, 0, 2, 3, 6]", nodes);

        CompetitionFloydWarshall fw = new CompetitionFloydWarshall("tinyEWD.txt", 75, 85, 95);
        nodes = Arrays.toString(fw.ewd.nodes().toArray());
        assertEquals("[4, 5, 7, 1, 0, 2, 3, 6]", nodes);
    }

    @Test
    public void testSetNodes() {
        CompetitionDijkstra cd = new CompetitionDijkstra("tinyEWD.txt", 60, 70, 80);
        String nodes = Arrays.toString(cd.ewd.nodes().toArray());
        assertEquals("[4, 5, 7, 1, 0, 2, 3, 6]", nodes);
        List<Integer> newNodes = new ArrayList<>();
        newNodes.add(7);
        newNodes.add(12);
        newNodes.add(23);
        newNodes.add(34);
        newNodes.add(45);
        newNodes.add(56);
        newNodes.add(67);
        newNodes.add(78);
        cd.ewd.setNodes(newNodes);
        nodes = Arrays.toString(cd.ewd.nodes().toArray());
        assertEquals("[7, 12, 23, 34, 45, 56, 67, 78]", nodes);

        CompetitionFloydWarshall fw = new CompetitionFloydWarshall("tinyEWD.txt", 75, 85, 95);
        nodes = Arrays.toString(fw.ewd.nodes().toArray());
        assertEquals("[4, 5, 7, 1, 0, 2, 3, 6]", nodes);
        newNodes = new ArrayList<>();
        newNodes.add(71);
        newNodes.add(62);
        newNodes.add(53);
        newNodes.add(44);
        newNodes.add(35);
        newNodes.add(26);
        newNodes.add(17);
        newNodes.add(8);
        fw.ewd.setNodes(newNodes);
        nodes = Arrays.toString(fw.ewd.nodes().toArray());
        assertEquals("[71, 62, 53, 44, 35, 26, 17, 8]", nodes);
    }

    /**
     * @param iter An Iterable object of DirectedEdges
     * @return A String representation of the Iterable object.
     */
    String iterString(Iterable<DirectedEdge> iter) {
        // This is an alternative to using Streams, which don't seem to be allowed in the version
        // of Java Webcat is using.
        List<DirectedEdge> arr = new ArrayList<>();
        for (DirectedEdge de : iter) {
            arr.add(de);
        }
        return Arrays.toString(arr.toArray());
    }
}

import java.util.*;
import java.util.stream.*;

public class App {
    public static void main(String[] args) throws Exception {
        // CompetitionDijkstra cd = new CompetitionDijkstra("input-D.txt", 50, 80, 60);
        // CompetitionDijkstra cd = new CompetitionDijkstra("input-I.txt", 72, 70, 65);
        // CompetitionDijkstra cd = new CompetitionDijkstra("input-K.txt", 51, 70, 88);
        // CompetitionDijkstra cd = new CompetitionDijkstra("input-A.txt", 50, 50, 50);
        // CompetitionDijkstra cd = new CompetitionDijkstra("input-A.txt", 60, 50, 75);
        CompetitionDijkstra cd = new CompetitionDijkstra("tinyEWD.txt", 50, 50, 50);
        // CompetitionDijkstra cd = new CompetitionDijkstra("1000EWD.txt", 50, 50, 50);
        System.out.println("Dijkstra: " + cd.timeRequiredforCompetition());
        // System.out.println(Arrays.toString(cd.dijkstra(6).toArray()));
        
        // CompetitionFloydWarshall fw = new CompetitionFloydWarshall("input-D.txt", 50, 80, 60);
        // CompetitionFloydWarshall fw = new CompetitionFloydWarshall("input-I.txt", 72, 70, 65);
        // CompetitionFloydWarshall fw = new CompetitionFloydWarshall("input-K.txt", 51, 70, 88);
        // CompetitionFloydWarshall fw = new CompetitionFloydWarshall("input-A.txt", 50, 50, 50);
        // CompetitionFloydWarshall fw = new CompetitionFloydWarshall("input-A.txt", 60, 50, 75);
        // CompetitionFloydWarshall fw = new CompetitionFloydWarshall("input-J.txt", 60, 50, 75);
        CompetitionFloydWarshall fw = new CompetitionFloydWarshall("tinyEWD.txt", 50, 69, 69);
        // CompetitionFloydWarshall fw = new CompetitionFloydWarshall("1000EWD.txt", 50, 50, 50);
        // fw.ewd.addEdge(new DirectedEdge(100, 200, 50));
        // System.out.println("Floyd-Warshall: " + fw.timeRequiredforCompetition());
        // int source = 4;
        // DijkstraSP sp = new DijkstraSP(fw.ewd, source);
        // System.out.println(sp.distTo(5));
        System.out.println(fw.ewd.edges());
        
        // System.out.println(Arrays.deepToString(fw.floydWarshall(fw.ewd)).replace("[", "{").replace("]", "}"));
        
    }
}

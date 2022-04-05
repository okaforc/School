import java.util.*;
// import java.io.*;

public class Project {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        List<String> exitPhrases = Arrays.asList(new String[] { "q", "quit", "exit", "-1" });
        // 1772369 edges
        // 8757 stops
        FileHandler.initStops("stops.txt");
        // FileHandler.initTimes("10000_stop_times.txt");
        FileHandler.initTimes("stop_times.txt");
        FileHandler.initTransfers("transfers.txt");
        System.out.println(
                "Welcome to the Vancouver public transport system API. There are various settings you can view: ");
        System.out.println(
                "\t1. Find the shortest path between two bus stops\n" +
                        "\t2. Search for a bus stop\n" +
                        "\t3. Search for a trip at a specified arrival time\n\n" +
                        "You may also enter -1 to exit.");
        main: while (true) {
            System.out.print("What would you like to do? ");
            String s = sc.nextLine();
            if (exitPhrases.contains(s)) {
                System.out.println("Goodbye.");
                break main;
            }
            try {
                switch (Integer.parseInt(s)) {
                    case 1:
                        // 1. Find the shortest path between two bus stops
                        System.out.print("Where are you departing from? Please enter the stop name in its entirety: ");
                        String res1 = sc.nextLine();
                        Integer t1 = -1, t2 = -1;
                        try {
                            t1 = Integer.parseInt(res1);
                        } catch (Exception e) {
                            // the user entry is a string, not an id
                            t1 = FileHandler.nameToID(res1);
                        }

                        if (t1 == null || !FileHandler.ewd.nodes().contains(t1)) {
                            System.out.println(
                                    "Sorry, that doesn't appear to be a valid bus stop. Try option 1 to see a list of matching bus stops.");
                            break;
                        }

                        System.out.print("Where are you getting off? Please enter the stop name in its entirety: ");
                        // int res2 = Integer.parseInt(sc.next());
                        String res2 = sc.nextLine();
                        try {
                            t2 = Integer.parseInt(res2);
                        } catch (Exception e) {
                            // the user entry is a string, not an id
                            t2 = FileHandler.nameToID(res2);
                        }

                        if (t2 == null || !FileHandler.ewd.nodes().contains(t2)) {
                            System.out.println(
                                    "Sorry, that doesn't appear to be a valid bus stop. Try option 1 to see a list of matching bus stops.");
                            break;
                        }

                        if (t1 == t2) {
                            System.out.println("You cannot travel to the same bus stop. Please choose a different bus stop as your start or end point.");
                            break;
                        }

                        DijkstraSP dj = new DijkstraSP(FileHandler.ewd, t1, FileHandler.maxValue);
                        System.out.println(FileHandler.namedPathTo(dj.pathTo(t2), 1));
                        System.out.println("\tCost: " + dj.distTo(t2) + "\n");

                        // FileHandler.ewd.addEdge(new DirectedEdge(646, 378, 1000));

                        // System.out.println(FileHandler.ewd.adj(646));
                        // System.out.println(a.equals(b));

                        break;
                    case 2:
                        // 2. Search for a bus stop
                        System.out.print("Please enter the name of your stop: ");
                        String stop = sc.nextLine();
                        int count = 0;
                        boolean hasMultiple = false, iterated = false;

                        // check to see if the query returns 0 results, 1 result, or multiple results
                        for (String throwaway : FileHandler.searchPrefix(FileHandler.getStopsTST(), stop)) {
                            iterated = true;
                            if (count > 1) {
                                hasMultiple = true;
                            }
                            count++;
                        }

                        if (!iterated) {
                            System.out.println("Your search query returned no results.");
                            break;
                        }

                        if (hasMultiple) {
                            System.out.printf("Your search query returned %s results: \n", count);
                        } else {
                            System.out.println("Your search query returned one result: ");
                        }

                        for (String str : FileHandler.searchPrefix(FileHandler.getStopsTST(), stop)) {
                            System.out.printf("- %s\n", FileHandler.get(FileHandler.getStopsTST(), str, 1, true));
                        }
                        break;

                    case 3:
                        // 3. Search for a trip at a specified arrival time
                        System.out.print(
                                "When would you like to arrive? Please enter your time in the format hh:mm:ss: ");
                        String time = sc.nextLine();
                        int count_t = 0;
                        boolean hasMultiple_t = false, iterated_t = false;

                        // check to see if the query returns 0 results, 1 result, or multiple results
                        for (String throwaway : FileHandler.searchb(FileHandler.getTimesTST(), time)) {
                            iterated_t = true;
                            if (count_t > 1) {
                                hasMultiple = true;
                            }
                            count_t++;
                        }

                        if (!iterated_t) {
                            System.out.println("Your search query returned no results.");
                            break;
                        }

                        if (hasMultiple_t) {
                            System.out.printf("Your search query returned %s results: \n", count_t);
                        } else {
                            System.out.println("Your search query returned one result: ");
                        }

                        System.out.printf("- %s\n", FileHandler.get_b(FileHandler.getTimesTST(), time, 1, true));
                        // System.out.printf("- %s\n", FileHandler.getTimesTST().get(time));
                        // for (String str : FileHandler.searchb(FileHandler.getTimesTST(), time)) {
                        //     // System.out.printf("- %s\n", FileHandler.get(FileHandler.getTimesTST(), str, 1, true));
                        // }
                        break;

                    default:
                        System.out.println("This is an invalid option (options are from 1-3). Please try again.\n");
                        break;
                }
                System.out.println("Options:");
                System.out.println(
                        "\t1. Find the shortest path between two bus stops\n" +
                                "\t2. Search for a bus stop\n" +
                                "\t3. Search for a trip at a specified arrival time\n\n" +
                                "You may also enter -1 to exit.");

            } catch (Exception e) {
                System.out.println("This is an invalid option. Please try again.\n");
                e.printStackTrace();
            }
        }

        sc.close();
    }
}

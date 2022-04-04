import java.util.*;
// import java.io.*;

public class Project {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        String[] arrExitPhrases = { "q", "quit", "exit", "-1" };
        List<String> exitPhrases = Arrays.asList(arrExitPhrases);
        // 1772369 edges
        // 8757 stops
        FileHandler.initStops("stops.txt");
        // FileHandler.initTimes("10000_stop_times.txt");
        FileHandler.initTimes("stop_times.txt");
        // FileHandler.initTransfers("transfers.txt");
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
                        // System.out.println("This hasn't been implemented yet, sorry!");
                        // System.out.println(FileHandler.ewd.edges());
                        // System.out.println(FileHandler.ewd.adj(1856));
                        System.out.print("Where are you departing from? Please enter the stop name in its entirety: ");
                        int res1 = Integer.parseInt(sc.next());
                        // String res1 = sc.nextLine();
                        System.out.print("Where are you getting off? Please enter the stop name in its entirety: ");
                        int res2 = Integer.parseInt(sc.next());
                        // String res2 = sc.nextLine();
                        // int t1 = FileHandler.idToName(FileHandler.)
                        DijkstraSP dj = new DijkstraSP(FileHandler.ewd, res1, FileHandler.maxValue);
                        System.out.println(FileHandler.namedPathTo(dj.pathTo(res2), 1));
                        
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

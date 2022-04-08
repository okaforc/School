import java.util.*;
// import java.io.*;

public class Project {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        List<String> exitPhrases = Arrays.asList("q", "quit", "exit", "-1");
        // 1772369 edges
        // 8757 stops
        // 5083 new edges
        FileHandler.initStops("stops.txt");
        // FileHandler.initTimes("10000_stop_times.txt");
        FileHandler.initTimes("stop_times.txt");
        FileHandler.initTransfers("transfers.txt");
        System.out.println(
                "Welcome to the Vancouver public transport system API. There are various settings you can view: ");
        System.out.println(
                """
                        \t1. Find the shortest path between two bus stops
                        \t2. Search for a bus stop
                        \t3. Search for a trip at a specified arrival time

                        You may also enter -1 to exit.""");
        while (true) {
            System.out.print("What would you like to do? ");
            String s = sc.nextLine();
            if (exitPhrases.contains(s)) {
                System.out.println("Goodbye.");
                break;
            }
            try {
                switch (Integer.parseInt(s)) {
                    case 1 -> {
                        // 1. Find the shortest path between two bus stops
                        System.out.print(
                                "Where are you departing from? Please enter the stop name (or ID) in its entirety: ");
                        String res1 = sc.nextLine().strip();
                        if (exitPhrases.contains(res1)) {
                            break;
                        }
                        Integer t1, t2;
                        try {
                            t1 = Integer.parseInt(res1.toUpperCase());
                        } catch (Exception e) {
                            // the user entry is a string, not an id
                            t1 = FileHandler.nameToID(res1.toUpperCase());
                        }
                        if (t1 == null || !FileHandler.ewd.nodes().contains(t1)) {
                            System.out.println(
                                    "Sorry, that doesn't appear to be a valid bus stop. Try option 1 to see a list of matching bus stops.");
                            break;
                        }
                        System.out.print(
                                "Where are you getting off? Please enter the stop name (or ID) in its entirety: ");
                        // int res2 = Integer.parseInt(sc.next());
                        String res2 = sc.nextLine().strip();
                        if (exitPhrases.contains(res2)) {
                            break;
                        }
                        try {
                            t2 = Integer.parseInt(res2.toUpperCase());
                        } catch (Exception e) {
                            // the user entry is a string, not an id
                            t2 = FileHandler.nameToID(res2.toUpperCase());
                        }
                        if (t2 == null || !FileHandler.ewd.nodes().contains(t2)) {
                            System.out.println(
                                    "Sorry, that doesn't appear to be a valid bus stop. Try option 2 to see a list of matching bus stops.");
                            break;
                        }
                        if (t2.equals(t1)) {
                            System.out.println(
                                    "You cannot travel to the same bus stop. Please choose a different bus stop as your start or end point.");
                            break;
                        }
                        DijkstraSP dj = new DijkstraSP(FileHandler.ewd, t1, FileHandler.maxValue);

                        String pt = FileHandler.namedPathTo(dj.pathTo(t2), 1);

                        if (pt == null) {
                            System.out.println("Unfortunately, there is no path from " + FileHandler.IDToName(t1)
                                    + " (stop no. " + t1 + ") to " + FileHandler.IDToName(t2) + " (stop no. " + t2
                                    + "). We apologise for the inconvenience.");
                        } else {
                            System.out.println(pt);
                            System.out.println("\tCost: " + dj.distTo(t2) + "\n");
                        }

                    }
                    case 2 -> {
                        // 2. Search for a bus stop
                        System.out.print("Please enter the name of your stop: ");
                        String stop = sc.nextLine().strip();
                        if (exitPhrases.contains(stop)) {
                            break;
                        }
                        int count = 0;
                        boolean hasMultiple = false, iterated = false;

                        // check to see if the query returns 0 results, 1 result, or multiple results

                        for (String str : FileHandler.searchPrefix(FileHandler.getStopsTST(), stop)) {
                            iterated = true;
                            if (count >= 1) {
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

                        // If there is one or multiple stops for this result but an exact search returns nothing, note tell user that there are duplicate names for stations.
                        // ! applies to duplicate stations
                        if (FileHandler.search(FileHandler.getStopsTST(), stop).size() != 0) {
                            System.out.println(
                                    "Please note that there may be multiple bus stops with the same name. In such a scenario, using the name of a bus stop may not give you your desired result (e.g. try entering only part of the name). \nFor more precise querying (e.g. option 1) please use the ID of the bus stop you are looking for.\n");
                        }
                    }
                    case 3 -> {
                        // 3. Search for a trip at a specified arrival time
                        System.out.print(
                                "When would you like to arrive? Please enter your time in the format hh:mm:ss: ");
                        String time = sc.nextLine().strip();
                        if (exitPhrases.contains(time)) {
                            break;
                        }
                        int count = 0;
                        boolean hasMultiple = false, iterated = false;

                        if (FileHandler.parseUserTime(time).length() > 8) {
                            // If the length of the returned String is greater than 8 (more characters than "hh:mm:ss"), then it must have returned an error message.
                            System.out.println(FileHandler.parseUserTime(time));
                            break;
                        }

                        // check to see if the query returns 0 results, 1 result, or multiple results
                        for (String str : FileHandler.search_time(FileHandler.getTimesTST(), time)) {
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
                        System.out.printf("%s\n", FileHandler.get_time(FileHandler.getTimesTST(), time, 1, true));
                    }
                    default ->
                        System.out.println("This is an invalid option (options are from 1-3). Please try again.\n");
                }
                System.out.println("Options:");
                System.out.println(
                        """
                                \t1. Find the shortest path between two bus stops
                                \t2. Search for a bus stop
                                \t3. Search for a trip at a specified arrival time

                                You may also enter -1 to exit.""");

            } catch (Exception e) {
                System.out.println("This is an invalid option. Please try again.\n");
            }
        }

        sc.close();
    }
}

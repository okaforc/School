import java.util.*;
import java.io.*;
import java.time.*;
import java.time.format.*;

public class FileHandler {
    public static final String[] movedKeywords = { "EB", "FLAGSTOP", "NB", "SB", "WB" };
    private static HashMap<Integer, String> stopInfo = new HashMap<>(); // stop id, stop name
    private static HashMap<String, String> prettyHead = new HashMap<>();
    private static TST<LinkedHashMap<String, String>> stopsTST = new TST<>();
    private static TST<ArrayList<LinkedHashMap<String, String>>> timesTST = new TST<>();
    private static String[] head;
    private static String[] headTimes;
    public static EdgeWeightedDigraph ewd;
    public static int maxValue;

    /**
     * Verify if a String {@code a} is in an array {@code arr} via a binary search.
     * @param arr The array to check for the string in.
     * @param a The string to check.
     * @return The index of the string in the array, or -1 if it isn't present.
     */
    public static int binarySearch(String[] arr, String a) {
        int lo = 0, hi = arr.length - 1;
        while (lo <= hi) {
            int mid = lo + (hi - lo) / 2;
            int cmp = a.compareTo(arr[mid]);

            if (cmp == 0)
                return mid;
            else if (cmp > 0)
                lo = mid + 1;
            else
                hi = mid - 1;
        }

        return -1; // not found
    }

    /**
     * {@brief} Populate the TST with the stops data found in {@code filename} and the edge weighted digraph associated with the files with nodes.
     * @param filename The name of a .txt/.csv file (must have a header and comma-separated values) with data intended for a TST.
     * @return -1 if an error occurs, else 0.
     */
    public static int initStops(String filename) {
        System.out.println("Loading stops...");
        ArrayList<Integer> nodes = new ArrayList<>();
        int maxVal = -1;
        try {
            BufferedReader br = new BufferedReader(
                    new InputStreamReader(new FileInputStream(filename), "UTF-8"));
            // br.readLine();
            String line = br.readLine();
            // ArrayList<Integer> stops = new ArrayList<>();

            head = line.split(","); // split the header by commas
            line = br.readLine();
            while (line != null) {
                String[] vals = line.split(","); // split each line by commas

                // add the stop ID to the graph as a node
                int val = Integer.parseInt(vals[0]);
                nodes.add(val);
                if (val > maxVal) maxVal = val;

                // in the event of a stop having no parent station, the empty value at that position isn't added. 
                // this adds an empty string to that position.
                if (vals.length < head.length) {
                    vals = Arrays.copyOf(vals, head.length);
                    vals[9] = "";
                }

                // create a new linkedhashmap for each node
                LinkedHashMap<String, String> m = new LinkedHashMap<>();
                for (int i = 0; i < head.length; i++) {
                    // add the data to the hashmap, with the header as the key and the value there as the value.
                    m.put(head[i], rewriteSpecialString(vals[i]));
                }

                // add the hashmap for each node with the stop name as the key to the TST
                stopsTST.put(m.get(head[2]), m);

                // add the stop's id (key) and name (value) to a lookup table
                stopInfo.put(Integer.parseInt(m.get(head[0])), m.get(head[2]));

                line = br.readLine(); // move onto the next line
            }

            initPrettyHead("stop");
            br.close();
        } catch (Exception e) {
            e.printStackTrace();
            return -1; // Return -1 if file name/data is invalid
        }

        ewd = new EdgeWeightedDigraph(nodes, maxVal);
        maxValue = maxVal;
        // Return 0 if code runs normally
        return 0;
    }

    /**
     * {@brief} Populate the TST with the stop times data found in {@code filename} and the edge weighted digraph associated with the files with edges.
     * @param filename The name of a .txt/.csv file (must have a header and comma-separated values) with data intended for a TST.
     * @return -1 if an error occurs, else 0.
     */
    
    public static int initTimes(String filename) {
        int percentageLoaded = 0;
        // private static int lines = 10001;
        int lines = 1772369;
        int portion = lines / 10;
        System.out.println("Loading stop times...");
        System.out.print("[");
        try {
            BufferedReader br = new BufferedReader(
                    new InputStreamReader(new FileInputStream(filename), "UTF-8"));
            String line = br.readLine();
            ArrayList<String> times = new ArrayList<>();
            int prevStopID = -1;
            boolean validStopTime;
            
            headTimes = line.split(","); // split the header by commas
            line = br.readLine();
            while (line != null) {
                validStopTime = true;
                String[] vals = line.split(","); // split each line by commas
                String time = "";

                // in the event of a stop having no parent station, the empty value at that position isn't added. 
                // this adds an empty string to that position.
                if (vals.length < headTimes.length) {
                    vals = Arrays.copyOf(vals, headTimes.length);
                    vals[8] = "";
                }

                // create a new linkedhashmap for each node
                LinkedHashMap<String, String> m = new LinkedHashMap<>();
                for (int i = 0; i < headTimes.length; i++) {
                    // add the data to the hashmap, with the header as the key and the parsed there as the value.
                    if (parseTime(vals[i].strip()) != null) {
                        m.put(headTimes[i], parseTime(vals[i].strip()));
                        if (i == 1) {
                            time = parseTime(vals[i].strip());
                        }
                    } else {
                        validStopTime = false;
                        // System.out.println("invalid stop time; discarding");
                        break;
                    }
                }

                if (validStopTime) {
                    if (!times.contains(time)) {
                        times.add(time);
                        // ArrayList<LinkedHashMap<String, String>> empty = new ArrayList<>();
                        timesTST.put(time, new ArrayList<LinkedHashMap<String, String>>());
                    }
                    ArrayList<LinkedHashMap<String, String>> t = timesTST.get(time);
                    t.add(m); // add the hashmap to the times arraylist
                    t = sortHMArr(t);
                    timesTST.put(time, t);

                    int curStopID = Integer.parseInt(vals[3]); // get stop_id value for current line
                    if (Integer.parseInt(vals[4]) != 1) {
                        // if this isn't the start of the trip, add an edge from the previous stop to the current one
                        ewd.addEdge(new DirectedEdge(prevStopID, curStopID, 1));
                    }
                    prevStopID = curStopID; // set this to the new previous stop id
                }

                percentageLoaded++;
                if (percentageLoaded == portion) {
                    System.out.print(String.valueOf(Character.toChars(0x2588)) + " "); // print full block chars while loading
                    percentageLoaded = 0;
                    // System.out.println(portion);
                }

                line = br.readLine(); // move onto the next line
            }

            initPrettyHead("time");
            br.close();
        } catch (Exception e) {
            e.printStackTrace();
            return -1; // Return -1 if file name/data is invalid
        }

        System.out.print("]\n");

        // Return 0 if code runs normally
        return 0;
    }



    /**
     * Populate the TST with the stops data found in {@code filename}.
     * @param filename The name of a .txt/.csv file (must have a header and comma-separated values) with data intended for a TST.
     * @return -1 if an error occurs, else 0.
     */
    public static int initTransfers(String filename) {
        System.out.println("Loading transfers...");
        try {
            BufferedReader br = new BufferedReader(
                    new InputStreamReader(new FileInputStream(filename), "UTF-8"));
            // br.readLine();
            String line = br.readLine();
            // ArrayList<Integer> stops = new ArrayList<>();

            head = line.split(","); // split the header by commas
            line = br.readLine();
            while (line != null) {
                String[] vals = line.split(","); // split each line by commas

                // in the event of a stop having no parent station, the empty value at that position isn't added. 
                // this adds an empty string to that position.
                if (vals.length < head.length) {
                    vals = Arrays.copyOf(vals, head.length);
                    vals[3] = "";
                }

                // create a new linkedhashmap for each node
                LinkedHashMap<String, String> m = new LinkedHashMap<>();
                for (int i = 0; i < head.length; i++) {
                    // add the data to the hashmap, with the header as the key and the value there as the value.
                    m.put(head[i], rewriteSpecialString(vals[i]));
                }

                // add the hashmap for each node with the stop name as the key to the TST
                stopsTST.put(m.get(head[2]), m);

                // add the stop's id (key) and name (value) to a lookup table
                stopInfo.put(Integer.parseInt(m.get(head[0])), m.get(head[2]));

                line = br.readLine(); // move onto the next line
            }

            initPrettyHead("stop");
            br.close();
        } catch (Exception e) {
            e.printStackTrace();
            return -1; // Return -1 if file name/data is invalid
        }

        

        // Return 0 if code runs normally
        return 0;
    }



    // Check is the first value of a string split space-wise is in the movedKeywords array.
    // If it is, move the value to the end of the string and return it. 
    // Otherwise, return the original string.
    private static String rewriteSpecialString(String str) {
        // if (binarySearch(movedKeywords, str.split(" ")[0]) != -1) {
        //     String a = str;
        //     List<String> val = new ArrayList<>(Arrays.asList(str.split(" ")));
        //     a = val.get(0);
        //     val.remove(0);
        //     val.add(a);
        //     return String.join(" ", val);
        // } else {
        //     return str;
        // }
        String res = str;
        int limit = res.split(" ").length;
        int count = 0;
        while (binarySearch(movedKeywords, res.split(" ")[0]) != -1 && count < limit) {
            String a = res;
            List<String> val = new ArrayList<>(Arrays.asList(res.split(" ")));
            a = val.get(0);
            val.remove(0);
            val.add(a);
            count++;
            res = String.join(" ", val);
        }
        return res;
    }

    // public static ArrayList<HashMap<String, String>> getStops() {
    //     return stops;
    // }

    private static void initPrettyHead(String type) {
        switch (type) {
            case "stop":
                prettyHead.put(head[0], "Stop ID");
                prettyHead.put(head[1], "Stop Code");
                prettyHead.put(head[2], "Stop Name");
                prettyHead.put(head[3], "Stop Description");
                prettyHead.put(head[4], "Stop Latitude");
                prettyHead.put(head[5], "Stop Longitude");
                prettyHead.put(head[6], "Zone ID");
                prettyHead.put(head[7], "Stop URL");
                prettyHead.put(head[8], "Location Type");
                prettyHead.put(head[9], "Parent Station");

                break;
            case "time":
                prettyHead.put(headTimes[0], "Trip ID");
                prettyHead.put(headTimes[1], "Arrival Time");
                prettyHead.put(headTimes[2], "Departure Time");
                prettyHead.put(headTimes[3], "Stop ID");
                prettyHead.put(headTimes[4], "Stop Sequence");
                prettyHead.put(headTimes[5], "Stop Headsign");
                prettyHead.put(headTimes[6], "Pickup Type");
                prettyHead.put(headTimes[7], "Drop-off Type");
                prettyHead.put(headTimes[8], "Shape Distance Travelled");

                break;

            default:
                break;
        }
    }

    /**
     * Returns the stopsTST saved to the FileHandler.
     * @return {@code stopsTST}
     */
    public static TST<LinkedHashMap<String, String>> getStopsTST() {
        return stopsTST;
    }

    /**
     * Returns the stopsTST saved to the FileHandler.
     * @return {@code timesTST}
     */
    public static TST<ArrayList<LinkedHashMap<String, String>>> getTimesTST() {
        return timesTST;
    }

    public static Iterable<String> secret() {
        return timesTST.keys();
    }

    /**
     * Allow user to search for station keys without case sensitivity.
     * @param prefix
     * @return The string returned by {@code TST.keysWithPrefix(String)} without case prejudice.
     */
    public static Iterable<String> searchPrefix(TST<LinkedHashMap<String, String>> tst, String prefix) {
        return tst.keysWithPrefix(prefix.toUpperCase());
    }

    /**
     * Allow user to search for station keys without case sensitivity.
     * @param prefix
     * @return The string returned by {@code TST.keysWithPrefix(String)} without case prejudice.
     */
    public static Iterable<String> search(TST<LinkedHashMap<String, String>> tst, String prefix) {
        return tst.keysThatMatch(prefix.toUpperCase());
    }

    public static Iterable<String> searchb(TST<ArrayList<LinkedHashMap<String, String>>> tst, String prefix) {
        return tst.keysThatMatch(prefix.toUpperCase());
    }

    /**
     * Get all the data of a specific key in the TST, with a specified amount of tabs.
     * @param key
     * @param tabs
     * @return The formatted string.
     */
    public static String get(TST<LinkedHashMap<String, String>> tst, String key, int tabs, boolean pretty) {
        if (key == null) {
            throw new IllegalArgumentException("calls get() with null argument");
        }
        if (key.length() == 0)
            throw new IllegalArgumentException("key must have length >= 1");
        LinkedHashMap<String, String> x = tst.get(key);
        if (x == null)
            return null;
        return mapToString(x, tabs, pretty, true);
    }

    public static String get_b(TST<ArrayList<LinkedHashMap<String, String>>> tst, String key, int tabs,
            boolean pretty) {
        if (key == null) {
            throw new IllegalArgumentException("calls get() with null argument");
        }
        if (key.length() == 0)
            throw new IllegalArgumentException("key must have length >= 1");
        ArrayList<LinkedHashMap<String, String>> x = tst.get(key);
        if (x == null)
            return null;
        String res = "";
        for (LinkedHashMap<String, String> hm : x) {
            res += mapToString(hm, tabs, pretty, false) + "\n";
        }
        return res;
    }

    /**
     * Return all formatted data of a string-string LinkedHashMap with tabs amount of tabs.
     * @param hm
     * @param tabs
     * @return The formatted string.
     */
    private static String mapToString(LinkedHashMap<String, String> hm, int tabs, boolean pretty, boolean which) {
        String res = "", tab = "";

        for (int i = 0; i < tabs; i++) {
            tab += "\t";
        }

        if (pretty) {
            for (int i = 0; i < hm.values().size(); i++) {
                if (which) {
                    res += tab + prettyHead.get(head[i]) + ": " + hm.get(head[i]) + "\n";
                } else {
                    res += tab + prettyHead.get(headTimes[i]) + ": " + hm.get(headTimes[i]) + "\n";

                }
            }
        } else {
            for (int i = 0; i < hm.values().size(); i++) {
                if (which) {
                    res += tab + head[i] + ": " + hm.get(head[i]) + "\n";
                } else {
                    res += tab + headTimes[i] + ": " + hm.get(headTimes[i]) + "\n";
                }
            }
        }

        return res;
    }

    private static String parseTime(String time) {
        if (!time.contains(":"))
            return time;

        String res = "";
        DateTimeFormatter timeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss");

        try {
            // add leading zeroes (e.g. 9 -> 09) to time if absent
            String[] temp = time.split(":");
            temp[0] = String.format("%02d", Integer.parseInt(temp[0]));
            res = String.join(":", temp);
            // parse the string to a time and return it if valid
            res = timeFormatter.format(
                    LocalTime.of(Integer.parseInt(temp[0]), Integer.parseInt(temp[1]), Integer.parseInt(temp[2])));
            // res = LocalTime.parse(res, timeFormatter).toString();
            return res;
        } catch (Exception e) {
            // if the string is not a valid time, return null
            return null;
        }
    }

    private static ArrayList<LinkedHashMap<String, String>> sortHMArr(ArrayList<LinkedHashMap<String, String>> list) {
        Collections.sort(list, new Comparator<LinkedHashMap<String, String>>() {
                    @Override
                    public int compare(LinkedHashMap<String, String> a, LinkedHashMap<String, String> b) {
                        return (a.get(headTimes[1]).compareTo(b.get(headTimes[1])));
                    }
                });
        return list;
    }

    public static ArrayList<DirectedEdge> arrayToList(Stack<DirectedEdge> arr) {
        ArrayList<DirectedEdge> p = new ArrayList<>();
        for (DirectedEdge directedEdge : arr) {
            p.add(directedEdge);
        }
        return p;
    }

    public static String namedPathTo(ArrayList<DirectedEdge> arr, int tabs) {
        String res = "";
        String tab = "";
        int i = 1;
        int last = 0;
        for (int j = 0; j < tabs; j++) {
            tab += "\t";
        }
        for (DirectedEdge de : arr) {
            res += tab + Integer.toString(i) + ". " + stopInfo.get(de.from()) + "\n";
            i++;
            last = de.to();
        }
        res += tab + Integer.toString(i) + ". " + stopInfo.get(last) + "\n\n";
        return res;
    }

    public static String idToName(int id) {
        return stopInfo.get(id);
    }
}

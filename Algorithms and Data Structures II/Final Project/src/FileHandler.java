import java.util.*;
import java.io.*;

public class FileHandler {
    public static final String[] movedKeywords = { "EB", "FLAGSTOP", "NB", "SB", "WB" };
    private static HashMap<String, String> prettyHead = new HashMap<>();
    // private static ArrayList<HashMap<String, String>> stops = new ArrayList<>();
    private static TST<LinkedHashMap<String, String>> trie = new TST<>();
    private static String[] head;

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

            if (cmp == 0) return mid;
            else if (cmp > 0) lo = mid + 1;
            else hi = mid - 1;
        }

        return -1; // not found
    }

    /**
     * Populate the TST with the data found in {@code filename}.
     * @param filename The name of a .txt/.csv file (must have a header and comma-separated values) with data intended for a TST.
     * @return -1 if an error occurs, else 0.
     */
    public static int initStops(String filename) {
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
                    vals[9] = "";
                }

                // create a new linkedhashmap for each node
                LinkedHashMap<String, String> m = new LinkedHashMap<>();
                for (int i = 0; i < head.length; i++) {
                    // add the data to the hashmap, with the header as the key and the value there as the value.
                    m.put(head[i], rewriteSpecialString(vals[i]));
                }

                // add the hashmap for each node with the stop name as the key to the TST
                trie.put(m.get(head[2]), m); 
                // stops.add(m);

                line = br.readLine(); // move onto the next line
            }

            initPrettyHead();
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

    private static void initPrettyHead() {
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
    }
    
    /**
     * Returns the trie saved to the FileHandler.
     * @return
     */
    public static TST<LinkedHashMap<String, String>> getTrie() {
        return trie;
    }

    /**
     * Allow user to search for station keys without case sensitivity.
     * @param prefix
     * @return The string returned by {@code TST.keysWithPrefix(String)} without case prejudice.
     */
    public static Iterable<String> searchPrefix(String prefix) {
        return trie.keysWithPrefix(prefix.toUpperCase());
    }

    /**
     * Get all the data of a specific key in the trie, with a specified amount of tabs.
     * @param key
     * @param tabs
     * @return The formatted string.
     */
    public static String get(String key, int tabs, boolean pretty) {
        if (key == null) {
            throw new IllegalArgumentException("calls get() with null argument");
        }
        if (key.length() == 0)
            throw new IllegalArgumentException("key must have length >= 1");
        LinkedHashMap<String, String> x = trie.get(key);
        if (x == null)
            return null;
        return mapToString(x, tabs, pretty);
    }

    /**
     * Return all formatted data of a string-string LinkedHashMap with tabs amount of tabs.
     * @param hm
     * @param tabs
     * @return The formatted string.
     */
    private static String mapToString(LinkedHashMap<String, String> hm, int tabs, boolean pretty) {
        String res = "", tab = "";

        for (int i = 0; i < tabs; i++) {
            tab += "\t";
        }
        if (pretty) {
            for (int i = 0; i < hm.values().size(); i++) {
                res += tab + prettyHead.get(head[i]) + ": " + hm.get(head[i]) + "\n";
            }
        } else {
            for (int i = 0; i < hm.values().size(); i++) {
                res += tab + head[i] + ": " + hm.get(head[i]) + "\n";
            }
        }

        return res;
    }
}

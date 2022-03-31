import java.util.*;
import java.io.*;

public class FileHandler {
    public static final String[] movedKeywords = { "EB", "FLAGSTOP", "NB", "SB", "WB" };
    // private static ArrayList<HashMap<String, String>> stops = new ArrayList<>();
    private static TST<HashMap<String, String>> trie = new TST<>();

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

            String[] head = line.split(","); // split the header by commas
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
        if (binarySearch(movedKeywords, str.split(" ")[0]) != -1) {
            String a = str;
            List<String> val = new ArrayList<>(Arrays.asList(str.split(" ")));
            a = val.get(0);
            val.remove(0);
            val.add(a);
            return String.join(" ", val);
        } else {
            return str;
        }
    }

    // public static ArrayList<HashMap<String, String>> getStops() {
    //     return stops;
    // }

    public static TST<HashMap<String, String>> getTrie() {
        return trie;
    }

    // allow user to search for strings without case sensitivity
    public static Iterable<String> searchPrefix(String prefix) {
        return trie.keysWithPrefix(prefix.toUpperCase());
    }
}

import java.util.*;
import java.io.*;

public class FileHandler {
    static String[] movedKeywords = {"flagstop", "wb", "nb", "sb", "eb"};
    private static ArrayList<HashMap<String, String>> stops = new ArrayList<>();

    /**
     * Verify if a String {@code a} is in an array {@code arr} via a binary search.
     * @param arr The array to check for the string in.
     * @param a The string to check.
     * @return
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

    public static int initStops(String filename) {
        try {
            BufferedReader br = new BufferedReader(
                    new InputStreamReader(new FileInputStream(filename), "UTF-8"));
            br.readLine();
            String line = br.readLine();
            // ArrayList<Integer> stops = new ArrayList<>();
            
            String[] head = line.split(",");
            line = br.readLine();
            while (line != null) {
                String[] vals = line.split(",");

                // in the event of a stop having no parent station, the empty value at that position isn't added. 
                // this adds an empty string to that position.
                if (vals.length < head.length) {
                    vals = Arrays.copyOf(vals, head.length);
                    vals[9] = "";   
                }
                
                LinkedHashMap<String, String> m = new LinkedHashMap<>();
                for (int i = 0; i < head.length; i++) {
                    // If one of the keywords in movedKeywords is in the string, move it to the end.
                    if (binarySearch(movedKeywords, vals[i].split(" ")[0]) != -1) {
                        List<String> val = new ArrayList<>(Arrays.asList(vals[i].split(" ")));
                        String a = val.get(0);
                        val.remove(0);
                        val.add(a);
                        vals[i] = String.join(" ", val);
                        
                    }
                    m.put(head[i], vals[i]);
                }
                stops.add(m);
                                
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

    public static ArrayList<HashMap<String, String>> getStops() {
        return stops;
    }
}

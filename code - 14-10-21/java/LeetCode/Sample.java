import java.util.ArrayList;
import java.util.Arrays;

public class Sample {
    public int arrayNesting(int[] nums) {
        int len = 0;
        ArrayList<Integer> fin = new ArrayList<>();
        Integer[] arr = new Integer[nums.length];
        Arrays.setAll(arr, i -> nums[i]);   // int[] to Integer[]

        boolean[] used = new boolean[arr.length];
        for (int i = 0; i < arr.length; i++) {
            fin.add(arr[arr[i]]);
            if (used[i] == true){
                continue;
            }
            // used[i] = true;
            for (int j = 0; j < arr.length; j++) {
                
                if (!fin.contains(arr[fin.get(j)])) {
                    fin.add(arr[fin.get(j)]);
                    used[Arrays.asList(arr).indexOf(arr[fin.get(j)])] = true;
                    System.out.println(Arrays.asList(arr).indexOf(arr[fin.get(j)]));
                } else {
                    used[j] = true;
                    System.out.println(fin);
                    System.out.println(Arrays.toString(used));
                    if (len < fin.size())
                        len = fin.size();
                    break;
                }
            }
            if (len == arr.length)
                return len;
            fin.clear();
        }
        return len;
    }
}

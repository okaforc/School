import java.util.Arrays;
import java.util.HashMap;


public class twoSumProblem {
    public static void main(String[] args) {
        int[] nums = { 3,3 };
        int target = 6;
        System.out.println(Arrays.toString(twoSum(nums, target)));
    }

    public static int[] twoSum(int[] nums, int target) {
        int[] ans = new int[2];
        HashMap<Integer, Integer> h = new HashMap<>();
        for (int i = 0; i < nums.length; i++) {
            int c = target - nums[i];
            if (h.containsKey(c)) {
                return new int[] {i, h.get(c)};
            }

            h.put(nums[i], i);
            
        }

        return ans;
    }
}

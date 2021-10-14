public class mode {
    public static void main(String[] args) {
        int[] nums = { 1, 31, 31, 31, 0 };
        int curr, inCurr;
        int maxOccur = 0;
        int currOccur = 0;
        int maxNum = 0;

        for (int i = 0; i < nums.length; i++) {
            currOccur = 0;
            curr = nums[i];
            for (int j = 0; j < nums.length; j++) {
                inCurr = nums[j];
                if (curr == inCurr) {
                    currOccur++;
                }
            }

            if (currOccur > maxOccur) {
                maxOccur = currOccur;
                maxNum = curr;
            }
        }

        System.out.println(maxNum);
    }
}

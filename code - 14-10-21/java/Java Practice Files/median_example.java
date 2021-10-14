import java.util.ArrayList;
import java.util.Collections;
import java.util.Scanner;

public class median_example {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.print("Enter a list of numbers separated by a comma (','): ");

        ArrayList<String> nums = new ArrayList<String>();
        for (String string : sc.nextLine().trim().split(",")) {
            nums.add(string.trim());
        }

        Collections.sort(nums);

        double median = 0;
        System.out.println(nums);
        int middleVal = (int) nums.size() / 2;  // will round up
        if (nums.size() % 2 == 1) {
            median = Double.parseDouble(nums.get(middleVal));
        } else {
            median = (Double.parseDouble(nums.get(middleVal)) + Double.parseDouble(nums.get(middleVal - 1))) / 2;
        }
        sc.close();

        String display = nums.get(0);

        for (int i = 1; i < nums.size(); i++) {
            display += ", " + nums.get(i);
        }

        System.out.printf("The median of %s is %.3f.", display, median);

    }
}

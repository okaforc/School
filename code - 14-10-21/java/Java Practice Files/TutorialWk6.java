import java.util.ArrayList;
import java.util.Collections;
import java.util.Scanner;

public class TutorialWk6 {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        ArrayList<Integer> nums = new ArrayList<Integer>();
        System.out.println("Enter your numbers separated by a comma (,): ");
        String userLine;
        do {
            userLine = sc.next();
            if(userLine.toLowerCase().equals("quit") || userLine.toLowerCase().equals("q")) {
                System.out.println("Bye.");
                break;
            }
            try {
                for (String num : userLine.split(",")) {
                    nums.add(Integer.parseInt(num.trim()));
                }

                Collections.sort(nums);
                System.out.printf("The minumum number is %d and the maximum number is %d.", nums.get(0),
                        nums.get(nums.size() - 1));

                System.out.println("\nEnter your numbers: ");
            } catch (NumberFormatException e) {
                System.out.println("You must input a series of integers, not letters.");
                break;
            }
        } while (sc.hasNext() || sc.nextLine().equals("quit"));

        sc.close();

    }
}

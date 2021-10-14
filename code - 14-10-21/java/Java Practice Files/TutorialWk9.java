import java.util.ArrayList;
import java.util.Scanner;

/**
 * TutorialWk9
 */
public class TutorialWk9 {

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        while (true) {
            try {
                System.out.print("Enter number: ");
                String userNum = sc.next();
                if (userNum.equalsIgnoreCase("exit") || userNum.equalsIgnoreCase("quit")) {
                    break;
                }
                int factNum = Integer.parseInt(userNum);
                if (isFactorian(factNum)) {
                    System.out.printf("The number %d is factorian.\n", factNum);
                } else {
                    System.out.printf("The number %d is NOT factorian.\n", factNum);
                }

            } catch (Exception e) {
                System.out.println("Invalid entry.");
            }
        }
        System.out.println("Goodbye.");
        sc.close();
    }

    public static long computeFactorial(int num) {
        if (num > 1) {
            return num * computeFactorial(num - 1);
        }
        return 1;
    }

    public static boolean isFactorian(int num) {
        ArrayList<Integer> numArray = new ArrayList<>();
        for (String s : String.valueOf(num).split("")) {
            numArray.add(Integer.parseInt(s));
        }

        int factorian = 0;

        for (Integer i : numArray) {
            factorian += computeFactorial(i);
        }

        if (factorian == num) {
            return true;
        }

        return false;
    }
}
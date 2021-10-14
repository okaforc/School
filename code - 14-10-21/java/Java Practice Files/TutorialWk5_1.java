import java.util.*;
// import java.util.Scanner;

public class TutorialWk5_1 {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        Integer userLimit, userMultiple;

        try {
            System.out.print("Enter your limit: ");
            userLimit = sc.nextInt();

            System.out.print("Enter your multiple: ");
            userMultiple = sc.nextInt();

            System.out.printf(String.format("The multiples of %d up to %d are ", userMultiple, userLimit)
                    + createMultiples(userLimit, userMultiple) + ".");

        } catch (InputMismatchException e) {
            System.out.println("You need to input an integer.");
        }

        sc.close();
    }

    public static String createMultiples(int limit, int multiple) {
        String nums = "0";
        int div = limit / multiple;

        for (int i = 1; i < div + 1; i++) {
            nums += ", " + String.valueOf(i * multiple);
        }
        return nums;
    }
}

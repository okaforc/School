
import java.util.Scanner;

public class IncrementalStatistics {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int numbersUsed = 1; // this is both the amount of numbers used, and the position (iterations)
        String userDouble; // user entered number as a string to use the sc.next() function
        double currentAverage = 0; // average at position n
        double oldAverage = 0; // average at position n-1
        double currentVariance = 0; // variance at position n. there is no oldVariance as it is not used in any
                                    // other line

        System.out.println("This program computes the average and variance of all numbers entered.");
        System.out.print("Enter a number (or type 'exit'): ");
        try {
            userDouble = sc.next();
            if (userDouble.equals("quit") || userDouble.equals("exit")) {
                System.out.println("Goodbye.");
                System.exit(0);
            }
            currentAverage = Double.parseDouble(userDouble);
            oldAverage = currentAverage;
            System.out.printf("So far the average is %.1f and the variance is %.1f.\n", currentAverage,
                    currentVariance);
        } catch (Exception e) {
            System.out.println("Error:  You must enter a real number (e.g. 12.5)");
            numbersUsed -= 1;
        }

        while (true) {
            try {

                numbersUsed++; // increase step with every iteration.
                System.out.print("Enter another number (or type 'exit'): ");
                userDouble = sc.next();
                if (userDouble.equals("quit") || userDouble.equals("exit")) {
                    // this works because userDouble == "quit" will always return false since two
                    // strings can never be the same unless they're the same object
                    System.out.println("Goodbye.");
                    break;
                }

                oldAverage = currentAverage;

                currentAverage = currentAverage + (Double.parseDouble(userDouble) - oldAverage) / numbersUsed;

                currentVariance = ((numbersUsed - 1) * currentVariance + ((Double.parseDouble(userDouble) - oldAverage)
                        * (Double.parseDouble(userDouble) - currentAverage))) / numbersUsed;

                System.out.printf("So far the average is %.1f and the variance is %.1f.\n", currentAverage,
                        currentVariance);
            } catch (Exception e) {
                System.out.println("Error:  You must enter a real number (e.g. 12.5)");
                numbersUsed -= 1;
            }
        }

        sc.close();

        
    }
}

/*  SELF ASSESSMENT of whether my code is easy to understand.
   1. Did I use easy-to-understand meaningful variable and constant names?
       Mark out of 10:   10
       Comment: I used useful and meaningful variable names.
   2. Did I format the variable and constant names properly (in lowerCamelCase and UPPERCASE)?
       Mark out of 5:   5
       Comment: I used lowerCamelCase to create the variable names.
   3. Did I indent the code appropriately?
       Mark out of 10:   10
       Comment: I used 4-space indents.
   4. Did I implement a for loop to read the input as required?
       Mark out of 10:   10
       Comment: I used a for loop.
      Total Mark out of  35 (Add all the previous marks):  35
*/

import java.util.InputMismatchException;
import java.util.Scanner;

public class TotalAverageCalculator {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        Integer userLimit, sumOfIntegers;
        double averageOfIntegers;

        try {
            userLimit = 0;
            sumOfIntegers = 0;
            averageOfIntegers = 0;

            System.out.print("\nHow many integers do you want to enter? ");
            userLimit = sc.nextInt();
            if (userLimit < 2 || userLimit > 10) {
                System.out.println(
                        "Error:  This program is constrained to only compute the total & average of between 2 & 10 integers.");
            } else {
                for (int i = 1; i < userLimit + 1; i++) {
                    System.out.printf("Enter integer %d: ", i);
                    sumOfIntegers += sc.nextInt();
                }
                averageOfIntegers = (double) sumOfIntegers / userLimit;
                System.out.printf("The sum of your integers is %d and the average is %.2f", sumOfIntegers,
                        averageOfIntegers);
            }
        } catch (InputMismatchException e) {
            System.out.println("You need to input an integer.");
        }

        sc.close();

    }
}

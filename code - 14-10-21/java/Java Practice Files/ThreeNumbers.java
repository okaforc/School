/*  SELF ASSESSMENT of whether my code is easy to understand.
   1. Did I use easy-to-understand meaningful and properly formatted variable names?
       Mark out of 5:   5
       Comment: All variable names are meaningful
    2. Did I indent the code appropriately?
       Mark out of 5:   5
       Comment: All indents are 4 spaces or on the next line is too long
   3. Did I make use of the functions appropriately within main and the other functions?
       Mark out of 10:   10
       Comment: I used four methods, one of which returns void.
       Total Mark out of  20 (Add all the previous marks):  20
*/

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;

public class ThreeNumbers {
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        boolean userHasQuit = false;

        while (!userHasQuit) {
            try {
                System.out.print("Please enter your three integers separated by spaces (or enter 'quit'):  ");

                ArrayList<String> userNumbers = new ArrayList<String>();
                for (String string : input.nextLine().trim().split(" ")) {
                    if (string.equals("quit")) {
                        input.close();
                        userHasQuit = true;
                    }
                    Integer.parseInt(string);
                    userNumbers.add(string.trim()); // remove all spaces
                }
                int firstUserNumber = Integer.parseInt(userNumbers.get(0));     // parse each string into integers
                int secondUserNumber = Integer.parseInt(userNumbers.get(1));
                int thirdUserNumber = Integer.parseInt(userNumbers.get(2));

                int medianOfUser = medianOf(firstUserNumber, secondUserNumber, thirdUserNumber);
                int countOfGreaterThanAverage = countOfNumbersGreaterThanTheAverage(firstUserNumber, secondUserNumber,
                        thirdUserNumber);

                System.out.println(getFormattedOutputString(medianOfUser, countOfGreaterThanAverage));

            } catch (Exception e) {
                // catch the exception and continue without an error message    
            }
        }

    }

    public static int medianOf(int a, int b, int c) {   
        int[] numbers = new int[] { a, b, c };
        Arrays.sort(numbers);
        return numbers[1];      // this will always be the median
    }

    public static double averageOf(int a, int b, int c) {
        double firstNum = (double) a;
        double secondNum = (double) b;
        double thirdNum = (double) c;
        double numberSum = 0;
        final double USER_NUMBERS = 3.0;
        numberSum = firstNum + secondNum + thirdNum;
        return numberSum / USER_NUMBERS;
    }

    public static int countOfNumbersGreaterThanTheAverage(int a, int b, int c) {
        double average = averageOf(a, b, c);
        int countOfNumbers = 0;
        int[] numbers = new int[] { a, b, c };
        for (int num : numbers) {
            if (num > average) {
                countOfNumbers++;
            }
        }
        return countOfNumbers;
    }

    public static String getFormattedOutputString(int median, int countOfGreaterThanAverage) {
        switch (countOfGreaterThanAverage) {
            case 0:
            case 1:
                return "The median of your numbers is " + median + ", and " + countOfGreaterThanAverage
                        + " of them is greater than their average.";

            default:
                return "The median of your numbers is " + median + ", and " + countOfGreaterThanAverage
                        + " of them are greater than their average.";

        }

    }
}

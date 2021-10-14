/*  SELF ASSESSMENT of whether my code is easy to understand.
   1. Did I use easy-to-understand meaningful and properly formatted variable names?
       Mark out of 5:   5
       Comment: I used camel case for variable names
    2. Is my code easy to follow/understand?
       Mark out of 5:   5
       Comment: I added comments for readability
   3. Did I use the functions as required?
       Mark out of 10:   10
       Comment: I used all required functions
       Total Mark out of  20 (Add all the previous marks):  20
*/

import java.util.Scanner;

public class PerniciousNumbers {
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        try {
            System.out.print("Enter the maximum number you want to consider: ");
            int userNumber = input.nextInt();
            for (int i = 0; i < userNumber + 1; i++) {
                if (isPernicious(i)) {
                    System.out.printf(
                            "%d is a pernicious number as it contains %d ones in it's binary representation (%s)\n", i,
                            countBinaryOnes(i), getBinaryString(i));
                }
            }
        } catch (Exception e) {
            // Catch the exception and exit quietly
        }
        input.close();
    }

    public static boolean isPrime(int num) {
        int primeIterations = 0; // prime numbers have only two factors: themselves and 1.
        for (int i = 1; i < num + 1; i++) {
            if (num % i == 0) {
                primeIterations++;
            }
        }
        if (primeIterations != 2) {
            return false;
        } else
            return true;
    }

    public static String getBinaryString(int num) {
        return num < 0 ? "-" + Long.toBinaryString(Math.abs(num)) : Long.toBinaryString(num);
    }

    public static int countBinaryOnes(int num) {
        String binNum = getBinaryString(num);
        int result = 0;
        for (Character binDigit : binNum.toCharArray()) {
            if (binDigit.equals('1')) {
                result++;
            }
        }
        return result;
    }

    public static boolean isPernicious(int num) {
        return isPrime(countBinaryOnes(num));
    }
}

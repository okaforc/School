import java.util.Scanner;

public class pernicious_example {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.print("Enter your number: ");
        Long p = sc.nextLong();
        String pBin = Long.toBinaryString(p);
        int result = 0; // number of 1's

        System.out.println(pBin.toCharArray());

        for (Character binDigit : pBin.toCharArray()) {
            // converts each char of pBin into a string, then into a number
            // this is because chars don't inherently have ordinary values like 0 or 1
            result += Integer.parseInt(Character.toString(binDigit));
        }

        if (isPrime(result))
            System.out.printf("Your number %d is a pernicious number.", p);
        else
            System.out.printf("Your number %d is NOT a pernicious number.", p);

        sc.close();
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

}

import java.util.Scanner;

public class LoanRepaymentCalculator {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        final int MONTHS_IN_YEAR = 12;
        double repay = 0;

        System.out.print("Enter loan amount? ");
        double principal = sc.nextDouble();

        System.out.print("Enter annual interest rate (e.g. 0.04)? ");
        double trueRate = sc.nextDouble();
        double rate = trueRate / MONTHS_IN_YEAR;

        System.out.print("Enter the term of the loan in years? ");
        int years = sc.nextInt();
        int months = years * MONTHS_IN_YEAR;

        sc.close();

        repay = principal * ((rate * Math.pow(1 + rate, months)) / (Math.pow(1 + rate, months) - 1));
        System.out.printf(
                "The monthly repayment for a %d year loan of %.2f at an annual interest rate of %.2f would be %.2f",
                years, principal, trueRate, repay);
    }
}

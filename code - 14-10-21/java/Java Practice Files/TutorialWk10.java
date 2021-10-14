import java.util.Arrays;
import java.util.Scanner;

public class TutorialWk10 {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        final byte AMOUNT_OF_PERCENTAGES = 5;
        double[] percentages = new double[AMOUNT_OF_PERCENTAGES];
        System.out.print("Enter the 5 percentages obtained: ");

        try {
            String[] rawPercentages = sc.nextLine().split(" ");
            for (int i = 0; i < rawPercentages.length; i++) {
                percentages[i] = Double.parseDouble(rawPercentages[i]);
            }

            System.out.printf("The average percentage is %.1f" + " and there was only %d above average student.",
                    determineAverageMark(percentages), countAboveAverageStudents(percentages));
        } catch (Exception e) {
            System.out.println("Invalid array");
        }

        sc.close();
    }

    public static double determineAverageMark(double[] marks) {
        double average = 0;
        for (double d : marks) {
            average += d;
        }
        return average / marks.length;
    }

    public static int countAboveAverageStudents(double[] marks) {
        int countAbove = 0;
        double average = determineAverageMark(marks);
        Arrays.sort(marks);
        for (double d : marks) {
            if (d > average) {
                countAbove++;
            }
        }
        return countAbove;
    }
}

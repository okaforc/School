import java.util.Scanner;

public class GradeComputation {
    public static final String[] MODULE_CODES = { "CSU11001", "CSU11010", "CSU11013", "CSU11021", "CSU11022",
            "CSU11026", "CSU11031", "CSU11081", "CSU12002", "STU11002" };
    public static final int[] MODULE_CREDITS = { 5, 10, 5, 5, 5, 10, 5, 5, 5, 5 };
    public static final int MIN_MARK = 40;

    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        final int MODULE_SIZE = MODULE_CODES.length;
        boolean userHasQuit = false;
        System.out.println("Welcome to the first year grade assessor.");
        while (true) {
            try {
                double[] marks = new double[MODULE_SIZE];

                for (int i = 0; i < marks.length; i++) {
                    try {
                        System.out.printf("Enter the student mark for %s (or enter quit): ", MODULE_CODES[i]);
                        String rawUserInput = input.next();
                        if (rawUserInput.equalsIgnoreCase("quit")) {
                            userHasQuit = true;
                            break;
                        }
                        double userNumber = Double.parseDouble(rawUserInput);
                        marks[i] = userNumber;

                    } catch (Exception e) {
                        System.out.println("Error - Enter a number between 0.0 and 100.0 or quit.");
                        i -= 1;
                    }
                }

                if (userHasQuit)
                    break;

                System.out.printf("Result = %s with an overall mark of %.0f" + "%s.\n", determineOverallGrade(marks),
                        weightedAverageMark(marks), "%");

                if (determineOverallGrade(marks).equals("FAIL")) {
                    System.out.printf("   %s credits were failed.\n", creditsBelowSpecifiedMark(marks, MIN_MARK));
                }

            } catch (Exception e) {
                System.out.println("Error - Enter a number between 0.0 and 100.0 or quit.");
            }
        }
        input.close();

    }

    public static int creditsBelowSpecifiedMark(double[] marks, int specifiedMinimumMark) {
        int creditsLost = 0;
        if (marks != null) {
            for (int i = 0; i < marks.length; i++) {
                if (Math.round(marks[i]) < specifiedMinimumMark) {
                    creditsLost += MODULE_CREDITS[i];
                }
            }
        }
        return creditsLost;
    }

    public static double weightedAverageMark(double[] marks) {
        double sum = 0;
        double totalDenominator = 0;
        if (marks.length != 0) {
            for (int i = 0; i < marks.length; i++) {
                sum += marks[i] * MODULE_CREDITS[i];
                totalDenominator += MODULE_CREDITS[i];
            }
            return sum / totalDenominator;
        }
        return 0;
    }

    public static String determineOverallGrade(double[] marks) {
        if (marks != null) {
            double average = Math.round(weightedAverageMark(marks));

            for (int i = 0; i < marks.length; i++) {

                if (marks[i] < 35) // if any mark is below 35, the student fails
                    return "FAIL";
            }
            if (creditsBelowSpecifiedMark(marks, 40) > 10) { 
                // if more than 10 credits are lost, the student fails
                return "FAIL";
            }
            if (average >= 70 && average <= 100) {
                return "I";
            }
            if (average >= 60 && average <= 69) {
                return "II.1";
            }
            if (average >= 50 && average <= 59) {
                return "II.2";
            }
            if (average >= 40 && average <= 49) {
                return "III";
            }
            if (average >= 0 && average <= 39) {
                return "FAIL";
            }
        }
        return null;
    }
}

import java.util.Scanner;

public class TutorialWk8 {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        try {
            System.out.print("Enter date (dd/mm/yyyy): ");
            String date = sc.next();
            sc.useDelimiter("/|\\-\n");
            String[] dateValues = date.split("/");
            int day = Integer.parseInt(dateValues[0]);
            int month = Integer.parseInt(dateValues[1]);
            int year = Integer.parseInt(dateValues[2]);

            boolean validDate = validDate(day, month, year);
            System.out.printf("The date %s is " + (validDate ? "valid." : "invalid."), date);

            sc.close();
        } catch (Exception e) {
            System.out.println("Invalid date.");
        }

    }

    public static int daysInMonth(int month, int year) {
        int daysInMonth = 0;

        switch (month) {
            case 1:
            case 3:
            case 5:
            case 7:
            case 8:
            case 10:
            case 12:
                daysInMonth = 31;
                break;

            case 4:
            case 6:
            case 9:
            case 11:
                daysInMonth = 30;
                break;
            case 2:
                daysInMonth = isLeapYear(year) ? 29 : 28;
                break;
            default:
                daysInMonth = -1;
                break;
        }

        return daysInMonth;
    }

    public static boolean isLeapYear(int year) {
        if (year % 400 == 0) {
            return true;
        } else if (year % 100 == 0) {
            return false;
        } else if (year % 4 == 0) {
            return true;
        } else {
            return false;
        }

    }

    public static boolean validDate(int day, int month, int year) {
        final int MONTHS_IN_YEAR = 12;

        if (day <= 0)
            return false;
        if (month <= 0 && month > MONTHS_IN_YEAR)
            return false;

        if (day > daysInMonth(month, year)) {
            return false;
        } else {
            return true;
        }

    }
}

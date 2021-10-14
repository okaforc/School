/*  SELF ASSESSMENT of whether my code is easy to understand.
   1. Did I use easy-to-understand meaningful variable names?
       Mark out of 5:   5
       Comment: I named all variables accordingly
   2. Did I format the variable names properly (in lowerCamelCase)?
       Mark out of 5:   5
       Comment: The user input variable is in lowerCamelCase
   3. Did I indent the code appropriately?
       Mark out of 5:   5
       Comment: All indents are 4 spaces wide and are aligned with each scope
   4. Did I implement a switch statement as required?
       Mark out of 10:   10
       Comment: I used a switch statement
       Total Mark out of  25 (Add all the previous marks):  25
*/

import java.util.Scanner;

public class Prizes {
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        boolean hasExited = false;
        String userPosition;
        System.out.println("This program tells competition participants what prize they have won.");

        while (!hasExited) {
            System.out.print("Enter your place number (or type 'exit'): ");
            userPosition = input.next();
            if (userPosition.equals("quit") || userPosition.equals("exit")) {
                hasExited = true;
                break;
            }
            try {
                switch (Integer.parseInt(userPosition)) {
                    case 1:
                        System.out.println(
                                "You came in 1st place and hence won two theatre tickets + drinks during the interval + dinner before the show.");
                        break;
                    case 2:
                        System.out.println(
                                "You came in 2nd place and hence won two theatre tickets + drinks during the interval.");
                        break;
                    case 3:
                        System.out.println("You came in 3rd place and hence won two theatre tickets.");
                        break;
                    case 4:
                    case 5:
                        System.out.printf("You came in %dth place and hence won a 10 Euro book token.\n",
                                Integer.parseInt(userPosition));
                        break;
                    case 6:
                    case 7:
                    case 8:
                    case 9:
                    case 10:
                        System.out.printf("You came in %dth place and hence won a 5 Euro book token.\n",
                                Integer.parseInt(userPosition));
                        break;
                    default:
                        System.out.println("Sorry.  You did not win a prize.");
                        break;
                }
            } catch (Exception e) {
                System.out.println("Sorry.  You did not win a prize.");
            }
        }

        input.close();
    }
}

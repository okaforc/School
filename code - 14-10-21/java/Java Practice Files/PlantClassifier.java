/*  SELF ASSESSMENT of whether my code is easy to understand.
   1. Did I use easy-to-understand meaningful variable names?
       Mark out of 10:   10
       Comment: All variable names have names closely related to their function or use
   2. Did I format the variable names properly (in lowerCamelCase)?
       Mark out of 5:   5
       Comment: All variables with sufficiently long names use lowerCamelCase
   3. Did I indent the code appropriately?
       Mark out of 15:   15
       Comment: All indents are 4 spaces each
      Total Mark out of  30 (Add all the previous marks):  30
*/

import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

public class PlantClassifier {
    public static void main(String[] args) {

        /*
         * Plants can be classified as ALGAE, BRYOPHYTES, PTERIDOPHYTES, GYMNOSPERMS and
         * ANGIOSPERMS. ALGAE have cells and tissues which are not highly organised.
         * BRYOPHYTES have cells and tissues which are organised into functional
         * structures, and have no vascular tissues. PTERIDOPHYTES have cells and
         * tissues which are organised into functional structures, have vascular tissues
         * and are dispersed by spores. GYMNOSPERMS have cells and tissues which are
         * organised into functional structures, have vascular tissues and are dispersed
         * by seeds where the seeds are not enclosed. ANGIOSPERMS have cells and tissues
         * which are organised into functional structures, have vascular tissues and are
         * dispersed by seeds where the seeds are enclosed.
         */

        Scanner scanner = new Scanner(System.in);
        List<String> yesAnswers = Arrays.asList("yes", "y"); // list arrays, which allow me to use the .contains(obj)
                                                             // function
        List<String> noAnswers = Arrays.asList("no", "n");
        String userAnswer;

        System.out.print(
                "Does the plant have cells and tissues which are organised into functional structures (Yes/No)? ");
        userAnswer = scanner.next().toLowerCase();

        do {
            if (noAnswers.contains(userAnswer)) {
                System.out.print("The plant is an ALGAE.");
            } else if (yesAnswers.contains(userAnswer)) {
                System.out.print("Does the plant have vascular tissues(Yes/No)? ");
                userAnswer = scanner.next().toLowerCase();

                if (noAnswers.contains(userAnswer)) {
                    System.out.print("The plant is a BRYOPHYTE.");
                } else if (yesAnswers.contains(userAnswer)) {
                    System.out.print("Is the plant dispersed by seeds (Yes/No)? ");
                    userAnswer = scanner.next().toLowerCase();

                    if (noAnswers.contains(userAnswer)) {
                        System.out.print("The plant is a PTERIDOPHYTE.");
                    } else if (yesAnswers.contains(userAnswer)) {
                        System.out.print("Are the seeds enclosed (Yes/No)? ");
                        userAnswer = scanner.next().toLowerCase();

                        if (noAnswers.contains(userAnswer)) {
                            System.out.print("The plant is a GYMNOSPERM.");
                        } else if (yesAnswers.contains(userAnswer)) {
                            System.out.print("The plant is an ANGIOSPERM.");
                        }
                    }
                }
            } else {
                System.out.println("Invalid input.");
                break;
            }

        } while (yesAnswers.contains(userAnswer) == false && noAnswers.contains(userAnswer) == false); // while answer
                                                                                                       // is in at least
                                                                                                       // one array.
                                                                                                       // otherwise,
                                                                                                       // exit.

        scanner.close();

    }
}

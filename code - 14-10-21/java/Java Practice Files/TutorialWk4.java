import java.util.*;

public class TutorialWk4 {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        List<String> yesStrings = Arrays.asList("yes", "Yes", "y", "Y");
        List<String> noStrings = Arrays.asList("no", "No", "n", "N");
        String ans;

        // "Does your vertebrate have fins (y/n)? "
        System.out.print("Does your vertebrate have fins (y/n)? ");
        ans = sc.next();
        do {
            if (yesStrings.contains(ans)) {
                System.out.println("Your vertabrate is: fish.");

            } else if (noStrings.contains(ans)) {
                System.out.print("Does your vertebrate have scales (y/n)? ");
                ans = sc.next();

                if (yesStrings.contains(ans)) {
                    System.out.println("Your vertabrate is: reptile");
                } else if (noStrings.contains(ans)) {
                    System.out.print("Does your vertebrate have moist skin (y/n)? ");
                    ans = sc.next();

                    if (yesStrings.contains(ans)) {
                        System.out.println("Your vertabrate is: frog");
                    } else if (noStrings.contains(ans)) {
                        System.out.print("Does your vertebrate have hair or fur (y/n)? ");
                        ans = sc.next();

                        if (yesStrings.contains(ans)) {
                            System.out.println("Your vertabrate is: mammal");
                        } else if (noStrings.contains(ans)) {
                            System.out.print("Does your vertebrate have feathers (y/n)? ");
                            ans = sc.next();

                            if (yesStrings.contains(ans)) {
                                System.out.println("Your vertabrate is: bird");
                            } else if (noStrings.contains(ans)) {
                                System.out.print("Your vertebrate isn't listed here.");
                            }
                        }
                    }
                }
            } else {
                System.out.println("Invalid input.");
                break;
            }

        } while (yesStrings.contains(ans) == false && noStrings.contains(ans) == false);

        sc.close();

    }
}

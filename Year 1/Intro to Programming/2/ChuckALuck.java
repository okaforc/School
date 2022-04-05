/* SELF ASSESSMENT 

1. ResolveBet

    I have correctly defined ResolveBet which takes the bet type (String) and the Wallet object, and a void return type [Mark out of 7: 7].
        Comment: I defined ResolveBet correctly
    My program presents the amount of cash in the wallet and asks the user how much he/she would like to bet [Mark out of 8: 8].
        Comment: i presented the amount of cash
    My program ensures the bet amount is not greater than the cash in the wallet [Mark out of 5: 5].
        Comment: I ensured the user could still pay
    My program creates three Dice objects, rolls them and creates a total variable with a summation of the roll values returned [Mark out of 15: 15]..
        Comment:i created three dice objects, rolled them and summed them
    My program determines the winnings by comparing the bet type with the total and comparing the bet type with the dice faces for the triple bet [Mark out of 20: 20].
        Comment: i determined the winnings
    My program outputs the results (win or loss) and adds the winnings to the wallet if user wins or removes the bet amount from the wallet if the user loses [Mark out of 10: 10].
        Comment: i returned the results

2. Main

    I ask the user for the amount of cash he/she has, create a Wallet object and put this cash into it [Mark out of 15:15 ]
        Comment:i put the user's money in the wallet object
    My program loops continuously until the user either enters quit or the cash in the wallet is 0 [Mark out of 5: 5]
        Comment: my program loops until the user can no longer or doesn't want to play
    I ask the user to enter any of the four bet types or quit [Mark out of 5: 5].
        Comment: i ask the user to enter a bet type
    My program calls resolveBet for each bet type entered [Mark out of 5: 5].
        Comment:i call ResolveBet for the bet type entered
    At the end of the game my program presents a summary message regarding winnings and losses [Mark out of 5: 5]
    Comment:my program summarises the user's total winnings

    Total Mark out of 100 (Add all the previous marks): 100
*/

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;

public class ChuckALuck {
    public static Scanner input = new Scanner(System.in);
    public static ArrayList<String> gameTypes = new ArrayList<String>(Arrays.asList("triple", "field", "high", "low"));
    public static ArrayList<String> quitPhrases = new ArrayList<String>(Arrays.asList("quit", "q", "exit"));
    public static double totalWinnings = 0;
    public static int wins = 0, losses = 0;

    public static void main(String[] args) {
        Wallet userMoney = new Wallet();
        String betType, userFirst;
        do {
            try {
                System.out.print("How much do you have in your wallet? ");
                userFirst = input.next();
                if (quitPhrases.contains(userFirst.toLowerCase())) {
                    System.out.println("Goodbye!");
                    System.exit(0);
                }
                if (Double.parseDouble(userFirst) < 0) {
                    throw new Exception("can't have negative money");
                }
                userMoney.put(Double.parseDouble(userFirst));
                break;
            } catch (Exception e) {
                // System.out.println(e);
                System.out.println("There appears to have been an error. Please try again.\n");
            }
        } while (true);

        do {
            try {
                if (Double.parseDouble(userFirst) <= 0) {
                    break;
                }
                System.out.printf("\nYou have $%.2f in cash remaining.\n", userMoney.check());
                System.out.print("\nWhat would you like to bet on (triple, field, high, low)? ");
                betType = input.next();
                if (quitPhrases.contains(userFirst.toLowerCase()) || quitPhrases.contains(betType.toLowerCase())) {
                    if (totalWinnings > 0) {
                        showSummary();
                        System.out.print(" Nice!\n");
                        System.out.println("Goodbye!");
                        System.exit(0);
                    } 
                    System.out.println("Goodbye!");
                    break;
                }
                if (userMoney.check() < 0 || !gameTypes.contains(betType.toLowerCase())) {
                    throw new Exception("no money left at this point somehow, or wrong game type");
                }
                ResolveBet(betType, userMoney);

            } catch (Exception e) {
                System.out.println("There appears to have been an error. Please try again.\n");
            }
        } while (userMoney.check() > 0);

        if (userMoney.check() <= 0) {
            showSummary();
            System.out.println("\n\nYou are out of money! You can no longer play. Goodbye!");
        }
        input.close();
    }

    public static void showSummary() {
        System.out.printf("You won %d %s, lost %d %s, and made $%.2f in winnings.", wins, wins > 1 ? "times" : "time", losses, losses > 1 ? "times" : "time", totalWinnings);
    }

    public static void ResolveBet(String betType, Wallet w) {
        int d1, d2, d3;
        double winnings = 0;
        Dice[] d = new Dice[3];

        System.out.print("How much would you like to bet? ");
        double betAmount = input.nextDouble();
        if (w.get(betAmount)) {
            for (int i = 0; i < d.length; i++) {
                d[i] = new Dice();
            }

            d1 = d[0].roll();
            d2 = d[1].roll();
            d3 = d[2].roll();
            int sum = d1 + d2 + d3;
            System.out.println("Sum: " + d1 + " + " + d2 + " + " + d3 + " = " + sum);

            switch (betType) {
                case "triple":
                    if ((d1 == d2 && d1 == d3) && d1 != 1 && d1 != 6) {
                        wins++;
                        winnings = 30 * betAmount;
                        w.put(winnings + betAmount); // make bet amount back plus winnings
                        totalWinnings += winnings;
                        System.out.printf("Congratulations! You have won the triple bet! You have recieved $%.2f.\n",
                                winnings);
                    } else {
                        losses++;
                        System.out.printf("Your triple bet failed! You have lost $%.2f.\n", betAmount);
                    }
                    break;
                case "field":
                    if (sum < 8 || sum > 12) {
                        wins++;
                        winnings = betAmount;
                        w.put(winnings + betAmount);
                        totalWinnings += winnings;
                        System.out.printf("Congratulations! You have won the field bet! You have recieved $%.2f.\n",
                                winnings);
                    } else {
                        losses++;
                        System.out.printf("Your field bet failed! You have lost $%.2f.\n", betAmount);
                    }
                    break;

                case "high":
                    if (sum > 10) {
                        if (!((d1 == d2 && d1 == d3) && d1 >= 4)) {
                            wins++;
                            winnings = betAmount;
                            w.put(winnings + betAmount);
                            totalWinnings += winnings;
                            System.out.printf("Congratulations! You have won the high bet! You have recieved $%.2f.\n",
                                    winnings);
                        }
                    } else {
                        losses++;
                        System.out.printf("Your high bet failed! You have lost $%.2f.\n", betAmount);
                    }
                    break;

                case "low":
                    if (sum < 11) {
                        if (!((d1 == d2 && d1 == d3) && d1 <= 4)) {
                            wins++;
                            winnings = betAmount;
                            w.put(winnings + betAmount);
                            totalWinnings += winnings;
                            System.out.printf("Congratulations! You have won the low bet! You have recieved $%.2f.\n",
                                    winnings);
                        }
                    } else {
                        losses++;
                        System.out.printf("Your low bet failed! You have lost $%.2f.\n", betAmount);
                    }
                    break;
                default:
                    break;
            }

        } else {
            System.out.println("You can't afford that!");
        }
    }
}

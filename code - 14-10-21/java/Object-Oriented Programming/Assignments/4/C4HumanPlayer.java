import java.util.Scanner;

public class C4HumanPlayer extends ConnectPlayer {
    Scanner sc = new Scanner(System.in);
    int userCol = 0;

    @Override
    public int chooseCol() {
        while (true) {
            try {
                System.out.printf("Player %c, enter the column you would like to place your piece (1-7): ", getID());
                userCol = sc.nextInt();
                if (grid.isValidColumn(userCol)) {
                    break;
                } else {
                    System.out.println("That is not a valid column. Try again.");
                }
            } catch (Exception e) {
                System.out.println("Error.");
                e.printStackTrace();
            }
        }
        return userCol;
    }

}

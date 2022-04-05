/* SELF ASSESSMENT

Connect4Game class (35 marks) 35
My class creates references to the Connect 4 Grid and two Connect 4 Players. It asks the user whether he/she would like to play/quit inside a loop. If the user decides to play then: 1. Connect4Grid2DArray is created using the Connect4Grid interface, 2. the two players are initialised - must specify the type to be ConnectPlayer, and 3. the game starts. In the game, I ask the user where he/she would like to drop the piece. I perform checks by calling methods in the Connect4Grid interface. Finally a check is performed to determine a win. 
Comment: I created all game objects and checked for wins after each piece was placed.

Connect4Grid interface (10 marks) 10
I define all 7 methods within this interface.
Comment: I defined all methods.

Connect4Grid2DArray class (25 marks)  25
My class implements the Connect4Grid interface. It creates a grid using a 2D array Implementation of the method to check whether the column to drop the piece is valid. It provides as implementation of the method to check whether the column to drop the piece is full. It provides as implementation of the method to drop the piece.  It provides as implementation of the method to check whether there is a win.
Comment: I used this class to implement all abilities of the game.

ConnectPlayer abstract class (10 marks) 10
My class provides at lest one non-abstract method and at least one abstract method. 
Comment: I have one abstract method and three non-abstract methods.

C4HumanPlayer class (10 marks) 10
My class extends the ConnectPlayer claas and overrides the abstract method(s). It provides the Human player functionality.
Comment: My class allows the user to input where they want to place their piece.

C4RandomAIPlayer class (10 marks) 10
My class extends the ConnectPlayer claas and overrides the abstract method(s). It provides AI player functionality. 
Comment: I used the java.util.Random class to pick a random number between 1 and 7 (incl.) and return it.

Total Marks out of 100: 100

*/

import java.util.*;

public class Connect4Game {

    public static Connect4Grid grid;

    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        String p1Type = "", p2Type = "";
        ConnectPlayer p1 = new C4HumanPlayer();
        ConnectPlayer p2 = new C4HumanPlayer();

        ArrayList<String> answers = new ArrayList<String>();
        answers.add("y");
        answers.add("yes");
        answers.add("n");
        answers.add("no");

        ArrayList<String> playerTypes = new ArrayList<String>();
        playerTypes.add("h");
        playerTypes.add("b");

        main: while (true) {
            try {
                System.out.print("Would you like to play? ");

                while (true) {
                    String ans = input.next();
                    if (answers.contains(ans)) {
                        if (ans.equalsIgnoreCase("n") || ans.equalsIgnoreCase("no")) {
                            System.out.println("Goodbye.");
                            break main;
                        } else {
                            break;
                        }
                    } else {
                        System.out.print("Invalid option. Please try again. ");
                    }
                }

            } catch (Exception e) {
                System.out.println("Invalid option. Please try again.");
            }

            grid = new Connect4Grid2DArray();

            while (true) {
                try {
                    System.out.print("Enter player one type (h = human, b = bot): ");
                    p1Type = input.next();

                    System.out.print("Enter player two type (h = human, b = bot): ");
                    p2Type = input.next();

                    if (playerTypes.contains(p1Type) && playerTypes.contains(p2Type)) {
                        break;
                    } else {
                        System.out.println("Invalid option. Please try again.");
                    }

                } catch (Exception e) {
                    System.out.println("Invalid option. Please try again.");
                }
            }

            int p1Col = 0;
            int p2Col = 0;

            if (p1Type.equalsIgnoreCase("h")) {
                p1 = new C4HumanPlayer();
            }
            
            if (p1Type.equalsIgnoreCase("b")) {
                p1 = new C4RandomAIPlayer();
            }

            if (p2Type.equalsIgnoreCase("h")) {
                p2 = new C4HumanPlayer();
            }

            if (p2Type.equalsIgnoreCase("b")) {
                p2 = new C4RandomAIPlayer();
            }

            p1.setID('1');
            p2.setID('2');
            grid.emptyGrid();

            while (!grid.isGridFull()) {
                p1Col = p1.chooseCol();
                if (grid.isColumnFull(p1Col)) {
                    System.out.println("p1, That column is full. Pick again.");
                    while (grid.isColumnFull(p1Col)) {
                        p1Col = p1.chooseCol();
                    }
                }

                p1.dropPiece(p1Col);

                if (grid.didLastPieceConnect4()) {
                    System.out.println(grid.toString());
                    System.out.println("Player 1 wins!\n\n");
                    break;
                }

                System.out.println(grid.toString());
                if (grid.isGridFull())
                    break;

                p2Col = p2.chooseCol();
                if (grid.isColumnFull(p2Col)) {
                    System.out.println("p2, That column is full. Pick again.");
                    while (grid.isColumnFull(p2Col)) {
                        p2Col = p2.chooseCol();
                    }
                }

                p2.dropPiece(p2Col);

                if (grid.didLastPieceConnect4()) {
                    System.out.println(grid.toString());
                    System.out.println("Player 2 wins!\n\n");
                    break;
                }

                System.out.println(grid.toString());

                if (grid.isGridFull())
                    break;
            }

            if (!grid.didLastPieceConnect4()) {
                System.out.println("It was a draw! No one wins, go home.\n\n");
            }
        }

        input.close();
    }

}
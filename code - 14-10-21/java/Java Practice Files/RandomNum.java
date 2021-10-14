import java.util.Random;
import java.util.Scanner; 

public class RandomNum {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        Random gen = new Random(1);
        final int MAX_NUMBER = 20;
        int GUESSES = 5;
        final int ORIG_GUESSES = GUESSES;
        int userNum;
        int guessNum = gen.nextInt(MAX_NUMBER+1);
        System.out.println(gen.nextInt(1));
        System.out.println(guessNum);

        do {
            System.out.print("Enter your number (0-20): ");
            userNum = sc.nextInt();
            checkNum(userNum, guessNum, ORIG_GUESSES, GUESSES);
            GUESSES -= 1;

            if(GUESSES < 0) {
                System.out.printf("You failed! The correct number was %d." , guessNum);
            }
        } while (GUESSES >= 0 && userNum != guessNum);
        sc.close();
    }

    public static boolean checkNum(int userNum, int guessNum, int o_guesses, int guesses) {
        boolean check = true;
        if(userNum == guessNum) {
            System.out.printf(o_guesses-guesses + 1 != 1 ? 
                "You got it right! You took %d guesses." : "You got it right! You took %d guess."
                , o_guesses-guesses+1);
        } else if (userNum > guessNum) {
            System.out.println("Too high.");
        } else System.out.println("Too low.");
        return check;
    }
}

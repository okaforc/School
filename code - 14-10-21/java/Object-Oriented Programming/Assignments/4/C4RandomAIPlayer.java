import java.util.Random;

public class C4RandomAIPlayer extends ConnectPlayer{

    @Override
    public int chooseCol() {
        Random rand = new Random();
        System.out.printf("Player %c, enter the column you would like to place your piece (1-7): ", getID());
        return rand.nextInt(7) + 1;
    }
}

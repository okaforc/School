import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class TutorialWk5_2 {
    public static void main(String[] args) {
        /*
         * 0 = Heads 1 = Tails
         */

        Random rand = new Random(234);
        List<String> heads = new ArrayList<String>();
        List<String> tails = new ArrayList<String>();
        final int SIDES = 2;

        for (int i = 1; i <= 10000; i++) {
            int flip = rand.nextInt(SIDES);

            if (flip == 0)
                heads.add(String.valueOf(flip));
            if (flip == 1)
                tails.add(String.valueOf(flip));
            if (i == 10000)
                System.out.println(String.format("The final toss resulted in a " + (flip == 0 ? "heads." : "tails.")));
            // System.out.println(flip);
        }

        System.out.println("There were " + heads.size() + " heads and " + tails.size() + " tails.");

    }
}

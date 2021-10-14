import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class TutorialWk7 {
    public static void main(String[] args) {
        int days = 12;

        for (int i = 1; i < days + 1; i++) {
            switch (i) {
                case 1:
                    System.out.println("On the 1st day of Christmas my true love gave to me: ");
                    System.out.println(printGifts(i));
                    break;
                case 2:
                    System.out.println("On the 2nd day of Christmas my true love gave to me: ");
                    System.out.println(printGifts(i));
                    break;
                case 3:
                    System.out.printf("On the 3rd day of Christmas my true love gave to me: ", i);
                    System.out.println(printGifts(i));
                    break;
                default:
                    System.out.printf("On the %dth day of Christmas my true love gave to me: ", i);
                    System.out.println("\n" + printGifts(i));
                    break;
            }

        }

    }

    public static String printGifts(int key) {
        String[] otherGifts = new String[] { "12 drummers drumming, ", "11 pipers piping, ", "10 lords a-leaping, ",
                "Nine ladies dancing, ", "Eight maids a-milking, ", "Seven swans a-swimming, ", "Six geese a-laying, ",
                "Five golden rings, ", "Four calling birds, ", "Three french hens, ", "Two turtle doves, and ",
                "a partridge in a pear tree." };
        ArrayList<String> gifts = new ArrayList<String>();
        for (String string : otherGifts) {
            gifts.add(string);
        }
        Collections.reverse(gifts);
        List<String> numGifts = gifts.subList(0, key);
        Collections.reverse(numGifts);

        String stringGifts = "";
        for (String string : numGifts) {
            stringGifts += string + "\n";
        }

        return stringGifts;
    }
}

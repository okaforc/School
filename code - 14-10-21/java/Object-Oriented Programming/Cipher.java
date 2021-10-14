import java.util.*;

public class Cipher {
    static String alpha = "abcdefghijklmnopqrstuvwxyz ";
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.print("Encrypt [e] or decrypt [d]? ");
        String choice = sc.nextLine();
        if (choice.equalsIgnoreCase("e")) {
            System.out.print("Enter phrase: ");
            String phrase = sc.nextLine();
            System.out.println("Your encrypted sentence is: " + encrypt(phrase.toCharArray(), createCipher()));
        } else if (choice.equalsIgnoreCase("d")) {
            System.out.print("Enter phrase: ");
            String phrase = sc.nextLine();
            System.out.println("Your decrypted sentence is: " + decrypt(phrase.toCharArray(), createCipher()));
        }
        sc.close();
    }
    
    public static char[] createCipher() {
        Random rand = new Random(1);
        int key = rand.nextInt(27);
        char[] alphaArr = alpha.toCharArray();
        char[] mapping = new char[27];
        
        for (char c : alphaArr) {
            int alphaIndex = alpha.indexOf(c);
            int transIndex = alphaIndex + key;
            if (transIndex >= alpha.length()) {
                transIndex -= alpha.length();
            }
            mapping[transIndex] = c;
        }
        return mapping;
    }
    
    public static String encrypt(char[] text, char[] mapping) {
        String map = Arrays.toString(mapping).replace("[", "").replace("]", "").replace(", ", "");
        String newMessage = "";
        for (char c : text) {
            c = Character.toLowerCase(c);
            int alIndex = map.indexOf(c);
            char newChar = alpha.charAt(alIndex);
            newMessage += newChar;
        }

        return newMessage;
    }

    public static String decrypt(char[] text, char[] mapping) {
        String map = Arrays.toString(mapping).replace("[", "").replace("]", "").replace(", ", "");
        String newMessage = "";
        for (char c : text) {
            c = Character.toLowerCase(c);
            int alIndex = alpha.indexOf(c);
            char newChar = map.charAt(alIndex);
            newMessage += newChar;
        }

        return newMessage;
    }
}

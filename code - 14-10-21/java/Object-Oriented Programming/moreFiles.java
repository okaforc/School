import java.io.FileWriter;
import java.io.IOException;
import java.util.Scanner;

public class moreFiles {
    Scanner sc = new Scanner(System.in);
    public static void main(String[] args) {
        try {
            FileWriter fw = new FileWriter("out.txt");
            fw.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        
    }

    void toDocument(FileWriter fw) {
        System.out.print("Enter word: ");
        String userPhrase = sc.nextLine().concat(" ");
        System.out.println(userPhrase);
    }
}

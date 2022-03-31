import java.util.*;
// import java.io.*;

public class Project {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        // System.out.println(sc.nextLine() + " was your input.");

        FileHandler.initStops("../stops.txt");
        // for (HashMap<String, String> hm : FileHandler.getStops()) {
        //     System.out.println(hm);
        // }
        // for (String hm : FileHandler.trie.keys()) {
        //     System.out.println(hm);
        // }
        System.out.print("Please enter the name of your stop: ");
        String s = sc.next();

        for (String str : FileHandler.searchPrefix(s)) {
            System.out.printf("\t- %s\n", str);
        }

        sc.close();
    }
}

import java.util.*;
// import java.io.*;

public class Project {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        // System.out.println(sc.nextLine() + " was your input.");

        FileHandler.initStops("../stops.txt");
        for (HashMap<String, String> hm : FileHandler.getStops()) {
            System.out.println(hm);
        }

        sc.close();
    }
}

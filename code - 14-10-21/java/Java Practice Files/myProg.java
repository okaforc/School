import java.util.Scanner;

public class myProg {
    public static void main(String[] args) {
        System.out.print("Name: ");
        Scanner scan = new Scanner(System.in);
        String name = scan.nextLine();
        scan.close();
        System.out.println("Hello " + name);

    }
}

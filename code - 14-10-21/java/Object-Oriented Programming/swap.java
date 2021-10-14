import java.util.Scanner;

public class swap {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        System.out.print("Enter number 1: ");
        int a = sc.nextInt();
        System.out.print("Enter number 2: ");
        int b = sc.nextInt();
        System.out.printf("Before swap: \n\ta = %d, \n\tb = %d\n\n", a, b);
        
        a = a + b;
        b = a - b;
        a = a - b;

        System.out.printf("After swap: \n\ta = %d, \n\tb = %d\n", a, b);

        sc.close();
    }
}


import java.util.Scanner;

public class Area {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        double area = 0;

        System.out.println("Enter first coordinates separated by a new line (ENTER or RETURN): ");
        double ax = sc.nextDouble();
        double ay = sc.nextDouble();

        System.out.println("Enter second coordinates: ");
        double bx = sc.nextDouble();
        double by = sc.nextDouble();
        
        System.out.println("Enter third coordinates: ");
        double cx = sc.nextDouble();
        double cy = sc.nextDouble();

        sc.close();
        area = Math.abs(
            (
                ax*(by-cy) + 
                bx*(cy-ay) +
                cx*(ay-by)
            ) / 2
        );

        System.out.printf("The area of your triangle is %.3f", area);
    }
}

import java.util.*;
import java.text.*;

public class practice {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        long Principal = 0;

        while (true) {
            System.out.print("Principal: ");
            Principal = scanner.nextLong();
            if(Principal>100000 || String.valueOf(Principal).length() > 8) {
                System.out.println("Too large");
            } else {
                break;
            }
        }
        

        System.out.print("Annual Interest Rate: ");
        float aiRate = scanner.nextFloat();

        System.out.print("Period (Years): ");
        byte period = scanner.nextByte();
    
        scanner.close();

        // returns mortgage (trust me)
        System.out.println(NumberFormat.getCurrencyInstance().format(Principal*(((aiRate/100/12)*Math.pow(1+aiRate/100/12, period*12))/(Math.pow(1+aiRate/100/12, period*12)-1))));
    }

    
}


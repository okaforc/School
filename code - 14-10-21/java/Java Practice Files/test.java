import java.util.Scanner;

public class test {
    public static void main(String[] args) {
        
        Scanner sc=new Scanner(System.in);
        String A=sc.next();
        String B=sc.next();
        sc.close();
        int lenA = A.length();
        int lenB = B.length();
        String alphabet = "abcdefghijklmnopqrstuvwxyz";
        // char charA = alphabet.indexOf(A.charAt(0));
        int charA = alphabet.indexOf(A.charAt(0));
        int charB = alphabet.indexOf(B.charAt(0));
        String upperA = A.substring(0, 1).toUpperCase() + A.substring(1);
        String upperB = B.substring(0, 1).toUpperCase() + B.substring(1);

        System.out.println(lenA + lenB);
        if(charA < charB) {
            System.out.println("No");
        } else{
            System.out.println("Yes");
        }

        System.out.println(upperA + " " + upperB);


        
    }
}

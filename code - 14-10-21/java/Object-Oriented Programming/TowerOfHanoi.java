import java.util.Scanner;

public class TowerOfHanoi {
    public static int moves = 0;
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.print("Enter number of disks: ");
        int userNum = sc.nextInt();
        executeHanoi(userNum, 'A', 'C', 'B');
        moves += 1;
        System.out.println("Number of moves: " + moves);
        sc.close();
    }
    
    public static void executeHanoi(int n, char fromR, char toR, char exR) {
        if (n == 1) {
            System.out.println("1: " + fromR + " --> " + toR);
            return;
        }
        executeHanoi(n-1, fromR, exR, toR);
        moves += 1;
        System.out.println(n + ": " + fromR + " --> " + toR);
        executeHanoi(n-1, exR, toR, fromR);
        moves += 1;
        
    }
}

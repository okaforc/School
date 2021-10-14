import java.util.*;

public class sandbox {
    public static Scanner input = new Scanner(System.in);

    static int[] arr = { 0 };

    public static void main(String[] args) {
        int[] arr1 = {10, 12, -14, 16}; 
        int[] arr2 = {8, 12, 18, 16}; 
        int n = arr1.length; 
        int m = arr2.length; 
        symmDiff(arr1, arr2, n, m); 
        input.close();
    }

    public static void temp(Object number) {

    }

    static void symmDiff(int[] arr1, int[] arr2, int n, int m) {
        // Traverse both arrays simultaneously.
        int i = 0, j = 0;
        while (i < n && j < m) {
            // Print smaller element and move
            // ahead in array with smaller element
            if (arr1[i] < arr2[j]) {
                System.out.print(arr1[i] + " ");
                i++;
            } else if (arr2[j] < arr1[i]) {
                System.out.print(arr2[j] + " ");
                j++;
            }

            // If both elements same, move ahead
            // in both arrays.
            else {
                i++;
                j++;
            }
        }
    }

    

}

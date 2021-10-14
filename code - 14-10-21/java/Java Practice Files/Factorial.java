public class Factorial {
    public static void main(String[] args) {
        System.out.println(findFactorial(10));
    }

    public static int findFactorial(int num) {
        if (num > 1) {
            return num * findFactorial(num-1);
        }
        return 1;
    }
}

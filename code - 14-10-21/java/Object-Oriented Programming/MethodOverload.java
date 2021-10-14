public class MethodOverload {
    public static void main(String[] args) {
        System.out.println("Integer: " + square(10));
        System.out.println("Float: " + square(10.3));
    }

    public static int square(int w) {
        return (int) Math.pow((double)w, (double)2);
    }

    public static double square(double w) {
        return Math.pow(w, 2);
    }
}

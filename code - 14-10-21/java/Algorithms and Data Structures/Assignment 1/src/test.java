import java.util.Arrays;

public class test {
    public static void main(String[] args) {
        In in1 = new In("input-files/input-files/r04000-1.txt");
        In in2 = new In("input-files/input-files/r04000-2.txt");
        In in3 = new In("input-files/input-files/r04000-3.txt");
        int[] a1 = in1.readAllInts();
        int[] a2 = in2.readAllInts();
        int[] a3 = in3.readAllInts();
        Stopwatch sw = new Stopwatch();
        int x = Collinear.countCollinear(a1, a2, a3);
        double time = sw.elapsedTime();
        // int x = Collinear.countCollinearFast(a1, a2, a3);
        /**
         * countCollinear:
         *      N = 1000
         *      T(N) = 1.69 + 1.64 + 1.653 / 3 == 1.661
         * 
         *      N = 2000
         *      T(N) = 15.221 + 15.418 + 15.188 /3 == 15.276
         * 
         *      N = 4000
         *      T(N) = 125.67 + 125.684 + 124.437 == 125.264
         */
        StdOut.print(x + "\n");
        StdOut.print("elapsed time: " + time);
    }
}

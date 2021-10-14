import java.util.ArrayList;
import java.util.Scanner;

public class stdv {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        double a = sc.nextDouble();
        double b = sc.nextDouble();
        double c = sc.nextDouble();
        sc.close();
        double sum = 0;
        double stdv = 0;
        ArrayList<Double> nums = new ArrayList<Double>();
        nums.add(a);
        nums.add(b);
        nums.add(c);

        for (Double d : nums) {
            sum += d;
        }
        double average = sum / nums.size();
        System.out.println("Average: " + average);

        for (Double d : nums) {
            stdv += Math.pow(d - average, 2);
        }
        stdv = Math.sqrt(stdv / nums.size());

        System.out.println("Standard Deviation: " + stdv);

    }
}

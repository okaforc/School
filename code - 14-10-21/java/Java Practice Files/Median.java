import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;

public class Median {
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        ArrayList<Double> rawNums = new ArrayList<Double>();
        System.out.println("Welcome to the median & rolling average system.");

        while (true) {
            try {
                System.out.print("Enter a number (or enter quit): ");
                String rawUserInput = input.next();
                if (rawUserInput.equalsIgnoreCase("quit")) {
                    break;
                }

                rawNums.add(Double.parseDouble(rawUserInput));
                double[] userNumbers = new double[rawNums.size()];

                for (int i = 0; i < userNumbers.length; i++) {
                    userNumbers[i] = rawNums.get(i);
                }

                System.out.printf("The median of %s is %.1f and the rolling average of the last 3 values is %.1f.\n",
                        convertToString(userNumbers), computeMedian(userNumbers),
                        computeRollingAverage(userNumbers, 3)); 
            } catch (Exception e) {
                System.out.println("Error - Enter any real number or quit.");
            }

        }

        input.close();

    }

    public static double computeMedian(double[] arr) {
        if (arr != null) {
            if (arr.length == 0) {
                return 0;
            }
            double[] sortArr = createSortedArray(arr);
            int middleVal = (int) (sortArr.length / 2); // will round up
            if (sortArr.length % 2 == 1) {
                return sortArr[middleVal];
            } else {
                return (sortArr[middleVal] + sortArr[middleVal - 1]) / 2;
            }
        }
        return 0;
    }

    public static double computeRollingAverage(double[] arr, int size) {
        double sumOfArray = 0;
        if (arr != null) {
            if (size == 0 || arr.length == 0) {
                return 0;
            } else if (arr.length < size) {
                // if the length of the array is too large, return the average of everything
                // in the array
                for (int i = 0; i < arr.length; i++) {
                    sumOfArray += arr[i];
                }
                return sumOfArray / (double) arr.length;
            } else { // otherwise, return the average of the last n doubles in the array
                for (int i = arr.length - size; i < arr.length; i++) {
                    sumOfArray += arr[i];
                }
                return sumOfArray / (double) size;
            }
        }
        return 0;
    }

    public static double[] createSortedArray(double[] arr) {
        if (arr != null) {
            double[] sortedArr = new double[arr.length];
            sortedArr = arr.clone();
            Arrays.sort(sortedArr);
            return sortedArr;
        }
        return arr;
    }

    public static String convertToString(double[] arr) {
        if (arr != null) {
            return Arrays.toString(arr).replace("[", "{ ").replace("]", " }");
        }
        return "{ }";
    }
}

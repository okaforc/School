public class pairwiseSum {
    public static void main(String[] args) {
        double[] finArr = {1, 2, 3, 4, 5, 6, 7, 8};

        System.out.println(pairSum(finArr, 8));
    }

    public static double pairSum(double[] arr, int size) {
        double sum = 0;
        double ts = size;
        double[] finArr = new double[(int)Math.ceil(ts/2)];
        int j = 0;
        for (int i = 0; i < size-1; i++) {
            if (i%2==0) {
                finArr[j] = arr[i] + arr[i+1];
                j++;
            }
        }

        if (finArr.length != 1) {
            pairSum(finArr, finArr.length);
        }

        sum = finArr[0];

        return sum;
    }
}

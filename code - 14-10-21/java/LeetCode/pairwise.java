public class pairwise {
    public static void main(String[] args) {
        int[] a = { 2, 4, 6, 8, 10, 12, 14, 16, 18, 20 };
        int x = pairSum(a);
        System.out.println(x);
    }

    public static int pairSum(int[] a) {
        int sum = 0, k = 0;
        int[] aux = new int[a.length / 2];

        if (a.length == 1){
            return a[0];
        }

        if (a.length == 2) {
            return a[0] + a[1];
        }

        for (int i = 0; i < a.length; i += 2) {
            if (i == a.length - 1) {
                aux[aux.length - 1] += a[i];
            } else {
                aux[k] = a[i] + a[i + 1];
            }
            k++;
        }

        sum = pairSum(aux);

        return sum;
    }
}

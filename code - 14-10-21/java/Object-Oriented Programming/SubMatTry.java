public class SubMatTry {

    /**
     * @param args
     */
    public static void main(String[] args) {
        int ans = 0;
        int a[][] = { 
                    { 3,  6, 12,  9,  0, 17,  2,  8 },
                    { 14, 45,  8, 13, 19, 23, 31,  3 },
                    { 5, 16, 37, 32, 74,  1, 66, 43 },
                    { 9, 13, 53, 27, 72, 43, 17, 19 },
                    { 15, 33, 65, 22,  4, 13, 12,  8 },
                    { 48, 16, 32, 96,  8,  4, 48,  0 },
                    { 27, 88, 92, 14,  6, 22, 77, 39 },
                    { 60, 71, 40, 91, 83, 22, 17, 13 }
                };
        int b[][] = { 
                        { 53, 27, 2 },  
                        { 65, 22,  4 },
                        { 32, 96,  8 }
                    };
        outerRow: for (int or = 0; or <= a.length - b.length; or++) {
            outerCol: for (int oc = 0; oc <= a[or].length - b[0].length; oc++) {
                for (int ir = 0; ir < b.length; ir++) {
                    for (int ic = 0; ic < b[ir].length; ic++) {
                        if (a[or + ir][oc + ic] != b[ir][ic]) {
                            continue outerCol;
                        }
                        // System.out.println(a[or + ir][oc + ic]);
                    }
                }
                // System.out.println("Submatrix found at row " + or + ", col " + oc);
                ans = 1;
                break outerRow;
            }
        }
        System.out.println(ans);
    }
}
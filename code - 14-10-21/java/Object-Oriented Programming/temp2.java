/**
 * temp2
 */
public class temp2 {

    public static void main(String[] args) {
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
                        { 53, 27, 72 },  
                        { 65, 22,  4 },
                        { 32, 96,  8 }
                    };
        int tempLoad1, tempLoad2;
        int aLimit = a.length - b.length;
        boolean isInArray = false;
        for (int or = 0; or <= aLimit && isInArray == false; or++) {
            for (int oc = 0; oc <= aLimit && isInArray == false; oc++) {
                isInArray = true;
                for (int ir = 0; ir < b.length && isInArray == true; ir++) {
                    for (int ic = 0; ic < b.length && isInArray == true; ic++) {
                        tempLoad1 = a[or + ir][ic + oc];
                        tempLoad2 = b[ir][ic];
                        if (tempLoad1 != tempLoad2) {
                            isInArray = false;
                        }
                    }
                }
            }
        }

        System.out.println(isInArray);
    }
}
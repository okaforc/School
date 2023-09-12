// -------------------------------------------------------------------------

/**
 *  This class contains static methods that implementing sorting of an array of numbers
 *  using different sort algorithms.
 *
 *  @author
 *  @version HT 2020
 */

import java.util.Arrays;

class SortComparison {

    /**
     * Sorts an array of doubles using InsertionSort.
     * This method is static, thus it can be called as SortComparison.sort(a)
     * @param a: An unsorted array of doubles.
     * @return array sorted in ascending order.
     *
     */
    static double[] insertionSort(double a[]) {
        if (a.length < 2) {
            return a; // if the array has 0 or 1 elements, it is already sorted.
        }

        // since this method returns an array instead of just sorting the given one and returning void,
        // we use a clone of the array and return that.
        double[] arr = a.clone();

        double cur;
        int j;

        for (int i = 1; i < arr.length; i++) {
            cur = arr[i];
            j = i;
            while (j > 0 && arr[j - 1] > cur) {
                arr[j] = arr[j - 1];
                j--;
            }
            arr[j] = cur;
        }

        return arr;
    }

    /**
    * Sorts an array of doubles using Selection Sort.
    * This method is static, thus it can be called as SortComparison.sort(a)
    * @param a: An unsorted array of doubles.
    * @return array sorted in ascending order
    *
    */
    static double[] selectionSort(double a[]) {
        if (a.length < 2) {
            return a;
        }

        double[] arr = a.clone();

        for (int i = 0; i < arr.length; i++) {
            int minindex = i;
            for (int j = i + 1; j < arr.length; j++) {
                if (arr[j] < arr[minindex]) {
                    minindex = j;
                }
            }
            exch(arr, minindex, i);
        }

        return arr;
    }//end selectionsort

    /**
     * Sorts an array of doubles using Quick Sort.
     * This method is static, thus it can be called as SortComparison.sort(a)
     * @param a : An unsorted array of doubles.
     * @return array sorted in ascending order
     *
     */
    static double[] quickSort(double a[]) {
        if (a.length < 2) {
            return a;
        }

        double[] arr = a.clone();
        quickHelper(arr, 0, arr.length - 1);
        return arr;
    }

    private static void quickHelper(double[] arr, int lo, int hi) {
        if (hi <= lo) {
            // if the pointer overlap or cross, then the array partition is sorted, so return
            return;
        }

        // get the partition index, then sort the two halves recursively
        int pivot = partition(arr, lo, hi);
        quickHelper(arr, lo, pivot - 1);
        quickHelper(arr, pivot + 1, hi);
    }


    /**
     * Given an array bounded by two variables, sort the array and return the index where every value to the left of it is
     * less than is and vice versa.
     * @param a : An unsorted array of doubles.
     * @param lo : The lower bound to sort the section of the array by.
     * @param hi : The upper bound to sort the section of the array by.
     * @return array sorted in ascending order
     *
     */
    static int partition(double[] a, int lo, int hi) {
        if (hi < 2) return 0;

        int i = lo, j = hi + 1;
        double pivot = a[lo]; // set default pivot value

        while (true) {
            while (a[++i] < pivot) {
                // increase i until it reaches hi
                if (i == hi) {
                    break;
                }
            }

            // move j down the array until its value reaches pivot
            j--; // equivalent to a[--j]. done differently for junit tests.
            while (pivot < a[j]) {
                j--;
            }

            if (i >= j) {
                break;
            }

            // swap values at i and j
            exch(a, i, j);
        }

        // return index where every value to left of j is smaller
        a[lo] = a[j];
        a[j] = pivot;
        return j;
    }

    /**
     * Sorts an array of doubles using Merge Sort.
     * This method is static, thus it can be called as SortComparison.sort(a)
     * @param a: An unsorted array of doubles.
     * @return array sorted in ascending order
     *
     */
    /**
     * Sorts an array of doubles using iterative implementation of Merge Sort.
     * This method is static, thus it can be called as SortComparison.sort(a)
     *
     * @param a: An unsorted array of doubles.
     * @return after the method returns, the array must be in ascending sorted order.
     */

    static double[] mergeSortIterative(double a[]) {
        if (a.length < 2) {
            return a;
        }

        int n = a.length;
        double[] arr = a.clone();
        double[] aux = a.clone();

        for (int i = 1; i < n; i = i + i) {
            for (int j = 0; j < n - i; j += i + i) {
                merge(arr, aux, j, j + i - 1, Math.min(j + i + i - 1, n - 1));
            }
        }

        return aux;
    }

    /**
     * Given an unsorted array of doubles and a copy of said array, merge sections of the array
     * given by a lower and upper bound, and a mid-point.
     * @param a : An unsorted array of doubles.
     * @param b : A copy of the inital array.
     * @param lo : The lower bound to sort the section of the array by.
     * @param hi : The upper bound to sort the section of the array by.
     * @param mid : The mid-point of the array section.
     * @return void
     *
     */
    private static void merge(double[] a, double[] b, int lo, int mid, int hi) {
        int i = lo, j = mid + 1, k = lo;

        // merge both halves of each array together
        while (i <= mid && j <= hi) {
            if (a[i] < a[j]) {
                b[k++] = a[i++];
            } else {
                b[k++] = a[j++];
            }
        }

        // copy any left over 
        while (i <= mid) {
            b[k++] = a[i++];
        }

        // fill in values
        for (i = lo; i <= hi; i++) {
            a[i] = b[i];
        }
        
    }

    /**
     * Sorts an array of doubles using recursive implementation of Merge Sort.
     * This method is static, thus it can be called as SortComparison.sort(a)
     *
     * @param a: An unsorted array of doubles.
     * @return after the method returns, the array must be in ascending sorted order.
     */
    static double[] mergeSortRecursive(double a[]) {
        double[] arr = a.clone();
        mergeHelper(a, arr, 0, arr.length - 1);
        return arr;
    }

    /**
     * Given an array bounded by two variables, sort the section of the array between the bounds/
     * @param a : An unsorted array of doubles.
     * @param a : A copy of the initial array.
     * @param lo : The lower bound to sort the section of the array by.
     * @param hi : The upper bound to sort the section of the array by.
     * @return void
     *
     */
    static void mergeHelper(double[] a, double[] b, int lo, int hi) {
        if (hi <= lo) {
            return;
        }

        int mid = lo + (hi - lo) / 2;
        mergeHelper(b, a, lo, mid);
        mergeHelper(b, a, mid + 1, hi);
        merge(a, b, lo, mid, hi);
    }

    /**
     * Given an array of doubles, swap the values at two indices.
     * @param arr : An array of doubles.
     * @param a : The index of the first double.
     * @param b : The index of the second double.
     * @return void
     *
     */
    static void exch(double[] arr, int a, int b) {
        double temp = arr[a];
        arr[a] = arr[b];
        arr[b] = temp;
    }

}//end class

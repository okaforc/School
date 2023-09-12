import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/**
 * Sorting Algorithm Times
 * The method used to get these result (which is written in a separate file), runTests(), runs the tests 3 times and 
 * averages the results.
 * Note that the method used to time the sorting algorithms, Stopwatch.java by Robert Sedgewick and Kevin Wayne,
 * measures in milliseconds. Any algorithm faster than 1 millisecond will return a time of 0 seconds.
 * 
 *            File                   |  Insertion |  Selection |   Quick  | Merge (Iter) | Merge(Recu)
 * numbers1000.txt                   |  0.006000  |  0.003333  | 0.000667 |   0.000333   | 0.000333
 * numbers1000Duplicates.txt         |  0.011000  |  0.002333  | 0.000333 |   0.000333   | 0.000333
 * numbers10000.txt                  |  0.032333  |  0.037000  | 0.001333 |   0.002333   | 0.002000
 * numbersNearlyOrdered1000.txt      |  0.000000  |  0.000667  | 0.000333 |   0.000000   | 0.000333
 * numbersReverse1000.txt            |  0.001000  |  0.000667  | 0.001333 |   0.000000   | 0.000000
 * numbersSorted1000.txt             |  0.000000  |  0.000667  | 0.001000 |   0.000333   | 0.000000
 * 
 * Questions
 * 		a. From my results, Insertion Sort is impacted the most, comparing numbersSorted1000.txt to numbersReverse1000.txt.
 * 			This is because the method must find the next largest number and switch it with the current one, iterating
 * 			over the entire array each time. If the array is sorted, the array is iterated through once and 
 * 			no swaps need to be made.
 * 		b. Insertion Sort has the biggest difference between a reversed and an already-sorted array. This is due to the same reason
 * 			as in part (a); a fully reversed array would require O(N^2) swaps and O(N^2) comparisons as it would have to loop
 * 			through the entire array for every element. For its best-case scenario, an already sorted array, it makes no swaps and 
 * 			only O(N) comparisons.
 * 		c. Selection sort appears to have the worst scalability
 * 		d. Recursive merge sort seems to be faster overall.
 * 		e. 	numbers1000.txt - Quick sort
 * 			numbers1000Duplicates.txt - Merge sort (iterative and recursive)
 * 			numbers10000.txt - Quick sort
 * 			numbersNearlyOrdered1000.txt - Insertion sort and merge sort (iterative)
 * 			numbersReverse1000.txt - Merge sort (iterative and recursive)
 * 			numbersSorted1000.txt - Insertion sort and merge sort (recursive)
 */

//-------------------------------------------------------------------------
/**
 *  Test class for SortComparison.java
 *
 *  @author
 *  @version HT 2020
 */
@RunWith(JUnit4.class)
public class SortComparisonTest {
	//~ Constructor ........................................................
	@Test
	public void testConstructor() {
		new SortComparison();
	}

	//~ Public Methods ........................................................

	// ----------------------------------------------------------
	/**
	 * Check that the methods work for empty arrays
	 */
	@Test
	public void testEmpty() {
		double[] arr = new double[0];

		double[] empty = new double[0];
		assertEquals(Arrays.toString(empty), Arrays.toString(SortComparison.selectionSort(arr)));
		assertEquals(Arrays.toString(empty), Arrays.toString(SortComparison.insertionSort(arr)));
		assertEquals(Arrays.toString(empty), Arrays.toString(SortComparison.quickSort(arr)));
		assertEquals(Arrays.toString(empty), Arrays.toString(SortComparison.mergeSortIterative(arr)));
		assertEquals(Arrays.toString(empty), Arrays.toString(SortComparison.mergeSortRecursive(arr)));

	}

	@Test
	public void testInsertion() {
		double[] arr;

		// 1 element
		arr = new double[] { 1 };
		assertEquals("[1.0]", Arrays.toString(SortComparison.insertionSort(arr)));
		// 2 elements
		arr = new double[] { -10, -213.3242453 };
		assertEquals("[-213.3242453, -10.0]", Arrays.toString(SortComparison.insertionSort(arr)));
		// already sorted, short
		arr = new double[] { -10, -4, 0, 1, 4.5, 5 };
		assertEquals("[-10.0, -4.0, 0.0, 1.0, 4.5, 5.0]", Arrays.toString(SortComparison.insertionSort(arr)));

		// mostly reverse
		arr = new double[] { 9, 8, 7, 6, 6, 5, 4, 4, 3, 2, 3, 2, 4, 4, 5, 1 };
		assertEquals("[1.0, 2.0, 2.0, 3.0, 3.0, 4.0, 4.0, 4.0, 4.0, 5.0, 5.0, 6.0, 6.0, 7.0, 8.0, 9.0]",
				Arrays.toString(SortComparison.insertionSort(arr)));
		// reverse sorted
		arr = new double[] { 9, 8, 7, 6, 6, 5, 5, 4, 3, 3, 3, 2, 2, 1, 1, 0 };
		assertEquals("[0.0, 1.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 4.0, 5.0, 5.0, 6.0, 6.0, 7.0, 8.0, 9.0]",
				Arrays.toString(SortComparison.insertionSort(arr)));
		// mostly sorted
		arr = new double[] { 1, 2, 3, 4, 3, 1, 6, 7, 8, 6, 9 };
		assertEquals("[1.0, 1.0, 2.0, 3.0, 3.0, 4.0, 6.0, 6.0, 7.0, 8.0, 9.0]",
				Arrays.toString(SortComparison.insertionSort(arr)));
		// already sorted, longer
		arr = new double[] { -1231.23, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
		assertEquals("[-1231.23, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0]",
				Arrays.toString(SortComparison.insertionSort(arr)));
	}

	@Test
	public void testSelection() {
		double[] arr;

		// 1 element
		arr = new double[] { -0 };
		assertEquals("[0.0]", Arrays.toString(SortComparison.selectionSort(arr)));
		// 3 elements
		arr = new double[] { -0, 0, 121 };
		assertEquals("[0.0, 0.0, 121.0]", Arrays.toString(SortComparison.selectionSort(arr)));

		// mostly reverse
		arr = new double[] { 9, 8, 7, 6, 6, 5, 4, 4, 3, 2, 3, 2, 4, 4, 5, 1 };
		assertEquals("[1.0, 2.0, 2.0, 3.0, 3.0, 4.0, 4.0, 4.0, 4.0, 5.0, 5.0, 6.0, 6.0, 7.0, 8.0, 9.0]",
				Arrays.toString(SortComparison.selectionSort(arr)));
		// reverse sorted
		arr = new double[] { 9, 8, 7, 6, 6, 5, 5, 4, 3, 3, 3, 2, 2, 1, 1, 0 };
		assertEquals("[0.0, 1.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 4.0, 5.0, 5.0, 6.0, 6.0, 7.0, 8.0, 9.0]",
				Arrays.toString(SortComparison.selectionSort(arr)));
		// mostly sorted
		arr = new double[] { 1, 2, 3, 4, 3, 1, 6, 7, 8, 6, 9 };
		assertEquals("[1.0, 1.0, 2.0, 3.0, 3.0, 4.0, 6.0, 6.0, 7.0, 8.0, 9.0]",
				Arrays.toString(SortComparison.selectionSort(arr)));
		// already sorted, longer
		arr = new double[] { -1231.23, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
		assertEquals("[-1231.23, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0]",
				Arrays.toString(SortComparison.selectionSort(arr)));
	}

	@Test
	public void testQuick() {
		double[] arr;

		// 1 element
		arr = new double[] { -0 };
		assertEquals("[0.0]", Arrays.toString(SortComparison.quickSort(arr)));
		// 3 elements
		arr = new double[] { -0, 0, 121 };
		assertEquals("[0.0, 0.0, 121.0]", Arrays.toString(SortComparison.quickSort(arr)));

		// mostly reverse
		arr = new double[] { 9, 8, 7, 6, 6, 5, 4, 4, 3, 2, 3, 2, 4, 4, 5, 1 };
		assertEquals("[1.0, 2.0, 2.0, 3.0, 3.0, 4.0, 4.0, 4.0, 4.0, 5.0, 5.0, 6.0, 6.0, 7.0, 8.0, 9.0]",
				Arrays.toString(SortComparison.quickSort(arr)));
		// reverse sorted
		arr = new double[] { 9, 8, 7, 6, 6, 5, 5, 4, 3, 3, 3, 2, 2, 1, 1, 0 };
		assertEquals("[0.0, 1.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 4.0, 5.0, 5.0, 6.0, 6.0, 7.0, 8.0, 9.0]",
				Arrays.toString(SortComparison.quickSort(arr)));
		// mostly sorted
		arr = new double[] { 1, 2, 3, 4, 3, 1, 6, 7, 8, 6, 9 };
		assertEquals("[1.0, 1.0, 2.0, 3.0, 3.0, 4.0, 6.0, 6.0, 7.0, 8.0, 9.0]",
				Arrays.toString(SortComparison.quickSort(arr)));
		// already sorted, longer
		arr = new double[] { -1231.23, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
		assertEquals("[-1231.23, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0]",
				Arrays.toString(SortComparison.quickSort(arr)));
	}

	@Test
	public void testMergeIter() {
		double[] arr;

		// 1 element
		arr = new double[] { -0 };
		assertEquals("[0.0]", Arrays.toString(SortComparison.mergeSortIterative(arr)));
		// 3 elements
		arr = new double[] { -0, 0, 121 };
		assertEquals("[0.0, 0.0, 121.0]", Arrays.toString(SortComparison.mergeSortIterative(arr)));

		// mostly reverse
		arr = new double[] { 9, 8, 7, 6, 6, 5, 4, 4, 3, 2, 3, 2, 4, 4, 5, 1 };
		assertEquals("[1.0, 2.0, 2.0, 3.0, 3.0, 4.0, 4.0, 4.0, 4.0, 5.0, 5.0, 6.0, 6.0, 7.0, 8.0, 9.0]",
				Arrays.toString(SortComparison.mergeSortIterative(arr)));
		// reverse sorted
		arr = new double[] { 9, 8, 7, 6, 6, 5, 5, 4, 3, 3, 3, 2, 2, 1, 1, 0 };
		assertEquals("[0.0, 1.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 4.0, 5.0, 5.0, 6.0, 6.0, 7.0, 8.0, 9.0]",
				Arrays.toString(SortComparison.mergeSortIterative(arr)));
		// mostly sorted
		arr = new double[] { 1, 2, 3, 4, 3, 1, 6, 7, 8, 6, 9 };
		assertEquals("[1.0, 1.0, 2.0, 3.0, 3.0, 4.0, 6.0, 6.0, 7.0, 8.0, 9.0]",
				Arrays.toString(SortComparison.mergeSortIterative(arr)));
		// already sorted, longer
		arr = new double[] { -1231.23, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
		assertEquals("[-1231.23, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0]",
				Arrays.toString(SortComparison.mergeSortIterative(arr)));
	}

	@Test
	public void testMergeRecu() {
		double[] arr;

		// 1 element
		arr = new double[] { -0 };
		assertEquals("[0.0]", Arrays.toString(SortComparison.mergeSortRecursive(arr)));
		// 3 elements
		arr = new double[] { -0, 0, 121 };
		assertEquals("[0.0, 0.0, 121.0]", Arrays.toString(SortComparison.mergeSortRecursive(arr)));

		// mostly reverse
		arr = new double[] { 9, 8, 7, 6, 6, 5, 4, 4, 3, 2, 3, 2, 4, 4, 5, 1 };
		assertEquals("[1.0, 2.0, 2.0, 3.0, 3.0, 4.0, 4.0, 4.0, 4.0, 5.0, 5.0, 6.0, 6.0, 7.0, 8.0, 9.0]",
				Arrays.toString(SortComparison.mergeSortRecursive(arr)));
		// reverse sorted
		arr = new double[] { 9, 8, 7, 6, 6, 5, 5, 4, 3, 3, 3, 2, 2, 1, 1, 0 };
		assertEquals("[0.0, 1.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 4.0, 5.0, 5.0, 6.0, 6.0, 7.0, 8.0, 9.0]",
				Arrays.toString(SortComparison.mergeSortRecursive(arr)));
		// mostly sorted
		arr = new double[] { 1, 2, 3, 4, 3, 1, 6, 7, 8, 6, 9 };
		assertEquals("[1.0, 1.0, 2.0, 3.0, 3.0, 4.0, 6.0, 6.0, 7.0, 8.0, 9.0]",
				Arrays.toString(SortComparison.mergeSortRecursive(arr)));
		// already sorted, longer
		arr = new double[] { -1231.23, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
		assertEquals("[-1231.23, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0]",
				Arrays.toString(SortComparison.mergeSortRecursive(arr)));
	}

	@Test
	public void testPartition() {
		double[] arr;
		int lo, hi;

		arr = new double[] {};
		lo = 0;
		hi = 0;
		assertEquals(0, SortComparison.partition(arr, lo, hi));

		arr = new double[] { 1 };
		lo = 0;
		hi = 0;
		assertEquals(0, SortComparison.partition(arr, lo, hi));

		arr = new double[] { 4, 6, 3, 7, 4 };
		lo = 0;
		hi = 4;
		assertEquals(2, SortComparison.partition(arr, lo, hi));

		arr = new double[] { 1 };
		lo = 0;
		hi = 1;
		assertEquals(0, SortComparison.partition(arr, lo, hi));

		arr = new double[] { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
		lo = 5;
		hi = 8;
		assertEquals(5, SortComparison.partition(arr, lo, hi));
	}

	@Test
	public void testExch() {
		double[] arr;
		int a, b;

		arr = new double[] { 1, 3 };
		a = 0;
		b = 1;
		SortComparison.exch(arr, a, b);
		assertEquals(3, arr[0], 0);

		arr = new double[] { 1, 3, 5, 7, 4, -2324, 92012.32103924810 };
		a = 4;
		b = 6;
		SortComparison.exch(arr, a, b);
		assertEquals(92012.32103924810, arr[4], 0);
	}

	@Test
	public void testMergeHelper() {
		// this method runs mergeSortRecursive but for selected sections of an array
		double[] x, y;
		int a, b;

		// sort a full array
		x = new double[] { 4, 7, 2, 7.8 };
		y = x.clone();
		a = 0;
		b = 3;

		SortComparison.mergeHelper(x, y, a, b);
		assertEquals("[2.0, 4.0, 7.0, 7.8]", Arrays.toString(y));

		// sort part of an array
		x = new double[] { -1212, 0, -2010000, 52, 9 };
		y = x.clone();
		a = 0;
		b = 2;

		SortComparison.mergeHelper(x, y, a, b);
		assertEquals("[-2010000.0, -1212.0, 0.0, 52.0, 9.0]", Arrays.toString(y));
	}


	// ----------------------------------------------------------
	/**
	 *  Main Method.
	 *  Use this main method to create the experiments needed to answer the experimental performance questions of this assignment.
	 * @throws IOException
	 *
	 */
	public static void main(String[] args) throws IOException {

		 /**
		 * numbers1000.txt: 8kb
		 * numbers1000Duplicates.txt: 4kb
		 * nubmers10000.txt: 87kb - use 128kb
		 * numbersNearlyOrdered1000kb: 9kb - use 16kb
		 * numbersReverse1000.txt: 9kb - use 16kb
		 * numbersSorted1000.txt: 9kb - use 16kb
		 */

		 /* int sigfig = 6; // significant figures to use in result
		 
		 App.runTests("numbers1000.txt", 8192, sigfig);
		 App.runTests("numbers1000Duplicates.txt", 4096, sigfig);
		 App.runTests("numbers10000.txt", 131072, sigfig);
		 App.runTests("numbersNearlyOrdered1000.txt", 16384, sigfig);
		 App.runTests("numbersReverse1000.txt", 16384, sigfig);
		 App.runTests("numbersSorted1000.txt", 16384, sigfig); */
		 
	}

}

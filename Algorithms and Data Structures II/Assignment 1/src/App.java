
import java.io.*;
import java.util.*;


public class App {
    public static void main(String[] args) throws IOException {
        
        /**
         * numbers1000.txt: 8kb - 8192
         * numbers1000Duplicates.txt: 4kb - 4096
         * nubmers10000.txt: 87kb - use 128kb - 131072
         * numbersNearlyOrdered1000.txt: 9kb - use 16kb - 16384
         * numbersReverse1000.txt: 9kb - use 16kb
         * numbersSorted1000.txt: 9kb - use 16kb
         */
        // double[] a = readToArray("numbersSorted1000.txt", 16384);
        int sigfig = 30;

		runTests("../numbers1000.txt", 8192, sigfig);
		runTests("../numbers1000Duplicates.txt", 4096, sigfig);
		runTests("../numbers10000.txt", 131072, sigfig);
		runTests("../numbersNearlyOrdered1000.txt", 16384, sigfig);
		runTests("../numbersReverse1000.txt", 16384, sigfig);
		runTests("../numbersSorted1000.txt", 16384, sigfig);
    }

    public static void runTests(String filename, int buf, int sigfig) throws IOException {
		double[] a = readToArray(filename, buf);
		double[] b = a.clone();

		System.out.println("File: " + filename + "\n");

		double t_ins = 0, t_sel = 0, t_quick = 0, t_mIter = 0, t_mRecu = 0;
		Stopwatch sw;

		sw = new Stopwatch();
		b = SortComparison.insertionSort(b);
		t_ins += sw.elapsedTime();
		b = a.clone();
		b = SortComparison.insertionSort(b);
		t_ins += sw.elapsedTime();
		b = a.clone();
		b = SortComparison.insertionSort(b);
		t_ins += sw.elapsedTime();
		System.out.printf("Insertion Sort: %." + sigfig + "f\n", t_ins / 3);

		b = a.clone();
		sw = new Stopwatch();
		b = SortComparison.selectionSort(b);
		t_sel = sw.elapsedTime();
		b = a.clone();
		sw = new Stopwatch();
		b = SortComparison.selectionSort(b);
		t_sel += sw.elapsedTime();
		b = a.clone();
		sw = new Stopwatch();
		b = SortComparison.selectionSort(b);
		t_sel += sw.elapsedTime();
		System.out.printf("Selection Sort: %." + sigfig + "f\n", t_sel / 3);

		b = a.clone();
		sw = new Stopwatch();
		b = SortComparison.quickSort(b);
		t_quick += sw.elapsedTime();
		b = a.clone();
		sw = new Stopwatch();
		b = SortComparison.quickSort(b);
		t_quick += sw.elapsedTime();
		b = a.clone();
		sw = new Stopwatch();
		b = SortComparison.quickSort(b);
		t_quick += sw.elapsedTime();
		System.out.printf("Quick Sort: %." + sigfig + "f\n", t_quick / 3);

		b = a.clone();
		sw = new Stopwatch();
		b = SortComparison.mergeSortIterative(b);
		t_mIter += sw.elapsedTime();
		b = a.clone();
		sw = new Stopwatch();
		b = SortComparison.mergeSortIterative(b);
		t_mIter += sw.elapsedTime();
		b = a.clone();
		sw = new Stopwatch();
		b = SortComparison.mergeSortIterative(b);
		t_mIter += sw.elapsedTime();
		System.out.printf("Merge Sort (Iterative): %." + sigfig + "f\n", t_mIter / 3);

		b = a.clone();
		sw = new Stopwatch();
		b = SortComparison.mergeSortRecursive(b);
		t_mRecu += sw.elapsedTime();
		b = a.clone();
		sw = new Stopwatch();
		b = SortComparison.mergeSortRecursive(b);
		t_mRecu += sw.elapsedTime();
		b = a.clone();
		sw = new Stopwatch();
		b = SortComparison.mergeSortRecursive(b);
		t_mRecu += sw.elapsedTime();
		System.out.printf("Merge Sort (Recursive): %." + sigfig + "f\n\n", t_mRecu / 3);
	}

	public static double[] readToArray(String filepath, int buf) throws IOException {
		List<Double> vals = new ArrayList<>();
		String currentLine = "";

		BufferedReader br = new BufferedReader(new FileReader(filepath), buf);

		while ((currentLine = br.readLine()) != null) {
			vals.add(Double.parseDouble(currentLine));
		}

		br.close();

		return vals.stream().mapToDouble(i -> i).toArray();
	}

    /* double[] a = { 1, 2, 3 };
        System.out.println("Testing Array    : " + Arrays.toString(a) + "\n" + a.length + " items\n");
        System.out.println("Insertion Sort   : " + Arrays.toString(SortComparison.insertionSort(a)));
        System.out.println("Selection Sort   : " + Arrays.toString(SortComparison.selectionSort(a)));
        System.out.println("Quick Sort       : " + Arrays.toString(SortComparison.quickSort(a)));
        System.out.println("Merge Sort (iter): " + Arrays.toString(SortComparison.mergeSortIterative(a)));
        System.out.println("Merge Sort (recu): " + Arrays.toString(SortComparison.mergeSortRecursive(a)));
         */
}

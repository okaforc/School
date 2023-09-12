/* 
	Slight edit: changed all "inp" to "../inp" to properly access adjacent directory.
*/

public class Search {
	
	static boolean isContained1(int[] A, int[] B) {

		boolean AInB = true;
		
		for (int i = 0; i < A.length; i++) {
			boolean iInB = linearSearch(B, A[i]);
			AInB = AInB && iInB;
		}
		
		return AInB;
	}

	static boolean isContained2(int[] A, int[] B) {
		
		int[] C = new int[B.length];
		
		for (int i = 0; i < B.length; i++) { C[i] = B[i]; }
		
		sort(C); // heapsort
		
		boolean AInC = true;
		for (int i = 0; i < A.length; i++) {
			boolean iInC = binarySearch(C, A[i]);
			AInC = AInC && iInC;
		}
		return AInC;
	}	
	
	static boolean linearSearch(int[] ar, int s) {
		boolean ret = false;
		for (int i = 0; i < ar.length; i++) {
			if (ar[i] == s) ret = true;
		}
		return ret;
	}	
	
	static boolean binarySearch(int[] a, int key) {
		int lo = 0, hi = a.length-1;
		
		while (lo <= hi) {
			int mid = lo + (hi - lo)/2;
			
			if (key < a[mid]) hi = mid - 1;
			else if (key > a[mid]) lo = mid + 1;
			else return true;
		}
		return false;
	}
	
	// Heapsort
	public static void sort(int[] a) {
		 int N = a.length;
		 for (int k = N/2; k >= 1; k--) sink(a, k, N);
		 while (N > 1) {
			 exch(a, 1, N);
			 sink(a, 1, --N);
		 }
	}
	private static void sink(int[] a, int k, int N) {
		 while (2*k <= N)
		 {
		 int j = 2*k;
		 if (j < N && (a[j] < a[j+1])) j++;
		 if (!(a[k] < a[j])) break;
		 exch(a, k, j);
		 k = j;
		 }
	}
	private static void exch(int[] a, int i, int j) {
		 int swap = a[i];
		 a[i] = a[j];
		 a[j] = swap;
	}

	public static void main(String[] args) {
		In in;
		int[] a, b;
		Stopwatch stopwatch;
		double time;
		/** Experiments for isContained1 **/

		System.out.println("--- isContained1 ---");
		// Experiment isContained1 with input size 500
		in = new In("../inp/a32000.txt");
		a = in.readAllInts();
		in = new In("../inp/b32000.txt");
		b = in.readAllInts();
		stopwatch = new Stopwatch();
		isContained1(a,b);
		time = stopwatch.elapsedTime();
		System.out.println("When N = " + a.length + ", elapsed time = " + time + " sec.");

		// Experiment isContained1 with input size 1000
		in = new In("../inp/a128000.txt");
		a = in.readAllInts();
		in = new In("../inp/b128000.txt");
		b = in.readAllInts();
		stopwatch = new Stopwatch();
		isContained1(a,b);
		time = stopwatch.elapsedTime();
		System.out.println("When N = " + a.length + ", elapsed time = " + time + " sec.");

	}
}

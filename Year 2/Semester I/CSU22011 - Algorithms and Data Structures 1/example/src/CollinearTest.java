import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.util.Arrays;

// !! Time spent so far: ~3h 30m


//-------------------------------------------------------------------------
/**
 * Test class for Collinear.java
 *
 * @author
 * @version 18/09/18 12:21:26
 */
@RunWith(JUnit4.class)
public class CollinearTest {
    // ~ Constructor ........................................................
    @Test
    public void testConstructor() {
        new Collinear();
    }

    // ~ Public Methods ........................................................

    // ----------------------------------------------------------
    /**
     * Check that the two methods work for empty arrays
     */
    @Test
    public void testEmpty() {
        int expectedResult = 0;

        assertEquals("countCollinear failed with 3 empty arrays", expectedResult,
                Collinear.countCollinear(new int[0], new int[0], new int[0]));
        assertEquals("countCollinearFast failed with 3 empty arrays", expectedResult,
                Collinear.countCollinearFast(new int[0], new int[0], new int[0]));
    }

    // ----------------------------------------------------------
    /**
     * Check for no false positives in a single-element array
     */
    @Test
    public void testSingleFalse() {
        int[] a3 = { 15 };
        int[] a2 = { 5 };
        int[] a1 = { 10 };

        int expectedResult = 0;

        assertEquals("countCollinear({10}, {5}, {15})", expectedResult, Collinear.countCollinear(a1, a2, a3));
        assertEquals("countCollinearFast({10}, {5}, {15})", expectedResult, Collinear.countCollinearFast(a1, a2, a3));
    }

    // ----------------------------------------------------------
    /**
     * Check for no false positives in a single-element array
     */
    @Test
    public void testSingleTrue() {
        int[] a3 = { 15, 5 };
        int[] a2 = { 5 };
        int[] a1 = { 10, 15, 5 };

        int expectedResult = 1;

        assertEquals(
                "countCollinear(" + Arrays.toString(a1) + "," + Arrays.toString(a2) + "," + Arrays.toString(a3) + ")",
                expectedResult, Collinear.countCollinear(a1, a2, a3));
        assertEquals("countCollinearFast(" + Arrays.toString(a1) + "," + Arrays.toString(a2) + "," + Arrays.toString(a3)
                + ")", expectedResult, Collinear.countCollinearFast(a1, a2, a3));
    }

    // TODO: add more tests here. Each line of code and each decision in
    // Collinear.java should be executed at least once from at least one test.

    // ----------------------------------------------------------
    /**
     * Check if the sorting algorithm works with both regular and empty arrays
     */
    @Test
    public void testSort() {
        int[] a = { 10, 213, 45, 4, -64, -400, 1, 111 };
        int[] expectedResult1 = { -400, -64, 1, 4, 10, 45, 111, 213 };

        int[] b = { 13964, -30060, -73731, 7947, -315, 504, -59, -4, 70, -87, 48, 25, 31, -2, 1, -8, 0 };
        int[] expectedResult2 = { -73731, -30060, -315, -87, -59, -8, -4, -2, 0, 1, 25, 31, 48, 70, 504, 7947, 13964 };

        int[] c = { 5795, -8785, -9081, 6945, -1203, -4882, 16, 5607, 6099, 6710, 8974, 1528, 6097, -850, -5642, 6269,
                -6629, -8400, -2403, 2863, 875, -7778, -397, -5163, -9132, 3751, 1233, 5881, -3579, 1795, 4946, -8471,
                -7368, 8304, -8759, -6262, -6307, 7922, -6646, -7984, -3955, -3158, 4882, -4337, 6773, -3545, -5124,
                -5062, 7433, 2574, -7498, 246, -1780, -5228, 5466, -1430, 7119, -3852, 1066, 2298, -2640, -8549, -8351,
                -387, -5778, 8990, 4774, 9896, 3345, 8361, -1417, -9540, 3541, -517, 2254, -7894, 2096, -3356, 271,
                8470, 3941, 2707, 8719, -5358, 7307, -8710, -2851, -7942, -9772, -7437, 6599, -970, 2663, -7769, -9946,
                -2375, -1660, -8868, 1119, -9278 };
        int[] expectedResult3 = { -9946, -9772, -9540, -9278, -9132, -9081, -8868, -8785, -8759, -8710, -8549, -8471,
                -8400, -8351, -7984, -7942, -7894, -7778, -7769, -7498, -7437, -7368, -6646, -6629, -6307, -6262, -5778,
                -5642, -5358, -5228, -5163, -5124, -5062, -4882, -4337, -3955, -3852, -3579, -3545, -3356, -3158, -2851,
                -2640, -2403, -2375, -1780, -1660, -1430, -1417, -1203, -970, -850, -517, -397, -387, 16, 246, 271, 875,
                1066, 1119, 1233, 1528, 1795, 2096, 2254, 2298, 2574, 2663, 2707, 2863, 3345, 3541, 3751, 3941, 4774,
                4882, 4946, 5466, 5607, 5795, 5881, 6097, 6099, 6269, 6599, 6710, 6773, 6945, 7119, 7307, 7433, 7922,
                8304, 8361, 8470, 8719, 8974, 8990, 9896 };

        int[] d = {};
        int[] expectedResult4 = {};

        // Sort arrays and then compare them, as the method sort() returns void
        Collinear.sort(a);
        Collinear.sort(b);
        Collinear.sort(c);
        Collinear.sort(d);

        assertArrayEquals(expectedResult1, a);
        assertArrayEquals(expectedResult2, b);
        assertArrayEquals(expectedResult3, c);
        assertArrayEquals(expectedResult4, d);
    }

    // ----------------------------------------------------------
    /**
     * Check if Binary Search works for regular and empty arrays
     */

    @Test
    public void testSearch() {
        int[] a1 = { -400, -64, 1, 4, 10, 45, 111, 213 };
        int a2 = -64;
        boolean expectedResult1 = true;

        int[] b1 = { -73731, -30060, -315, -87, -59, -8, -4, -2, 0, 1, 25, 31, 48, 70, 504, 7947, 13964 };
        int b2 = 13964;
        boolean expectedResult2 = true;

        int[] c1 = { -9946, -9772, -9540, -9278, -9132, -9081, -8868, -8785, -8759, -8710, -8549, -8471,
                -8400, -8351, -7984, -7942, -7894, -7778, -7769, -7498, -7437, -7368, -6646, -6629, -6307, -6262, -5778,
                -5642, -5358, -5228, -5163, -5124, -5062, -4882, -4337, -3955, -3852, -3579, -3545, -3356, -3158, -2851,
                -2640, -2403, -2375, -1780, -1660, -1430, -1417, -1203, -970, -850, -517, -397, -387, 16, 246, 271, 875,
                1066, 1119, 1233, 1528, 1795, 2096, 2254, 2298, 2574, 2663, 2707, 2863, 3345, 3541, 3751, 3941, 4774,
                4882, 4946, 5466, 5607, 5795, 5881, 6097, 6099, 6269, 6599, 6710, 6773, 6945, 7119, 7307, 7433, 7922,
                8304, 8361, 8470, 8719, 8974, 8990, 9896 };
        int c2 = -105;
        boolean expectedResult3 = false;

        int[] d1 = {};
        int d2 = 69;
        boolean expectedResult4 = false;

        assertEquals("Collinear.binarySearch(a1)", expectedResult1, Collinear.binarySearch(a1, a2));
        assertEquals("Collinear.binarySearch(b1)", expectedResult2, Collinear.binarySearch(b1, b2));
        assertEquals("Collinear.binarySearch(c1)", expectedResult3, Collinear.binarySearch(c1, c2));
        assertEquals("Collinear.binarySearch(d1)", expectedResult4, Collinear.binarySearch(d1, d2));
    }


    public static void main(String[] args) {
        
    }
}

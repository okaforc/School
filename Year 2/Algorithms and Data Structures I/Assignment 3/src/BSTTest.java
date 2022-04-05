import static org.junit.Assert.*;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

//-------------------------------------------------------------------------
/**
 *  Test class for Doubly Linked List
 *
 *  @version 3.1 09/11/15 11:32:15
 *
 *  @author  Chike Okafor
 */

@RunWith(JUnit4.class)
public class BSTTest {

    /** <p>Test {@link BST#prettyPrintKeys()}.</p> */

    @Test
    public void testPrettyPrint() {
        BST<Integer, Integer> bst = new BST<Integer, Integer>();
        assertEquals("Checking pretty printing of empty tree", "-null\n", bst.prettyPrintKeys());

        //  -7
        //   |-3
        //   | |-1
        //   | | |-null
        //   | |  -2
        //   | |   |-null
        //   | |    -null
        //   |  -6
        //   |   |-4
        //   |   | |-null
        //   |   |  -5
        //   |   |   |-null
        //   |   |    -null
        //   |    -null
        //    -8
        //     |-null
        //      -null

        bst.put(7, 7);
        bst.put(8, 8);
        bst.put(3, 3);
        bst.put(1, 1);
        bst.put(2, 2);
        bst.put(6, 6);
        bst.put(4, 4);
        bst.put(5, 5);

        String result = "-7\n" + " |-3\n" + " | |-1\n" + " | | |-null\n" + " | |  -2\n" + " | |   |-null\n"
                + " | |    -null\n" + " |  -6\n" + " |   |-4\n" + " |   | |-null\n" + " |   |  -5\n"
                + " |   |   |-null\n" + " |   |    -null\n" + " |    -null\n" + "  -8\n" + "   |-null\n"
                + "    -null\n";
        assertEquals("Checking pretty printing of non-empty tree", result, bst.prettyPrintKeys());
    }

    /** <p>Test {@link BST#delete(Comparable)}.</p> */
    @Test
    public void testDelete() {
        BST<Integer, Integer> bst = new BST<Integer, Integer>();
        bst.delete(1);
        assertEquals("Deleting from empty tree", "()", bst.printKeysInOrder());

        bst.put(7, 7); //        _7_
        bst.put(8, 8); //      /     \
        bst.put(3, 3); //    _3_      8
        bst.put(1, 1); //  /     \
        bst.put(2, 2); // 1       6
        bst.put(6, 6); //  \     /
        bst.put(4, 4); //   2   4
        bst.put(5, 5); //        \
                       //         5

        assertEquals("Checking order of constructed tree", "(((()1(()2()))3((()4(()5()))6()))7(()8()))",
                bst.printKeysInOrder());

        bst.delete(9);
        assertEquals("Deleting non-existent key", "(((()1(()2()))3((()4(()5()))6()))7(()8()))", bst.printKeysInOrder());

        bst.delete(8);
        assertEquals("Deleting leaf", "(((()1(()2()))3((()4(()5()))6()))7())", bst.printKeysInOrder());

        bst.delete(6);
        assertEquals("Deleting node with single child", "(((()1(()2()))3(()4(()5())))7())", bst.printKeysInOrder());

        bst.delete(3);
        assertEquals("Deleting node with two children", "(((()1())2(()4(()5())))7())", bst.printKeysInOrder());

        bst.delete(7);
        assertEquals("Deleting root", "((()1())2(()4(()5())))", bst.printKeysInOrder());
    }

    /** <p>Test {@link BST#isEmpty()}.</p> */
    @Test
    public void testIsEmpty() {
        BST<Integer, Integer> bst = new BST<Integer, Integer>();
        assertTrue("Checking emptiness - expect true", bst.isEmpty());

        bst.put(0, 34);
        assertFalse("Checking emptiness - expect false", bst.isEmpty());
        bst.put(1, 43);
        assertFalse("Checking emptiness - expect false", bst.isEmpty());
        bst.deleteMax();
        bst.deleteMax();
        assertTrue("Checking emptiness - expect true", bst.isEmpty());
    }

    /** <p>Test {@link BST#size()}.</p> */
    @Test
    public void testSize() {
        BST<Integer, Integer> bst = new BST<Integer, Integer>();
        assertEquals("Checking size of BST - expecting 0", 0, bst.size());
        bst.put(0, 10);
        assertEquals("Checking size of BST - expecting 1", 1, bst.size());
        bst.put(1, 20);
        bst.put(2, 30);
        bst.put(3, 40);
        bst.put(4, 50);
        bst.put(5, 60);
        assertEquals("Checking size of BST - expecting 6", 6, bst.size());
        bst.delete(2);
        assertEquals("Checking size of BST - expecting 5", 5, bst.size());
        bst.delete(4);
        bst.deleteMax();
        assertEquals("Checking size of BST - expecting 3", 3, bst.size());
    }

    /** <p>Test {@link BST#contains(Comparable)}.</p> */
    @Test
    public void testContains() {
        BST<Integer, Integer> bst = new BST<Integer, Integer>();
        assertFalse("Checking if BST contains any elements - expecting false", bst.contains(2));
        assertFalse("Checking if BST contains any elements - expecting false", bst.contains(0));
        assertFalse("Checking if BST contains any elements - expecting false", bst.contains(291382847));
        bst.put(-3, 1);
        assertTrue("Checking if BST contains -3 - expecting true", bst.contains(-3));
        bst.put(-3, 1);
        bst.put(105, 14);
        assertTrue("Checking if BST contains 105 - expecting true", bst.contains(105));

        BST<String, Integer> bst2 = new BST<String, Integer>();
        bst2.put("hello world", 3);
        assertFalse("Checking if BST contains \"helloworld\" - expecting false", bst2.contains("helloworld"));
        assertTrue("Checking if BST contains \"hello world\" - expecting true", bst2.contains("hello world"));
    }

    /** <p>Test {@link BST#get(Comparable)}.</p> */
    @Test
    public void testGet() {
        BST<Integer, Integer> bst = new BST<Integer, Integer>();
        assertEquals("Getting non-existant value", null, bst.get(3));
        assertEquals("Getting non-existant value", null, bst.get(-3));
        bst.put(0, 1);
        bst.put(1, 2);
        bst.put(2, 3);
        bst.put(3, 4);
        bst.put(4, 5);
        assertEquals("Getting key value", (Integer) 4, bst.get(3));
        assertEquals("Getting key value", (Integer) 1, bst.get(0));
        assertEquals("Getting non-existant key value", null, bst.get(7));
        bst.put(5, -502);
        assertEquals("Getting key value", (Integer) (-502), bst.get(5));
        bst.put(5, 47);
        assertEquals("Getting key value", (Integer) 47, bst.get(5));
    }

    /** <p>Test {@link BST#put(Comparable)}.</p> */
    @Test
    public void testPut() {
        BST<Integer, Integer> bst = new BST<Integer, Integer>();
        assertFalse("Checking if contains non-existant key", bst.contains(2));
        assertFalse("Checking if contains non-existant key", bst.contains(null));
        bst.put(120, 21);
        bst.put(9, 7);
        bst.put(-12, -12133);
        assertTrue("Checking if contains key 120", bst.contains(120));
        assertTrue("Checking if contains key 9", bst.contains(9));
        assertFalse("Checking if contains key 78", bst.contains(78));
        assertTrue("Checking if contains key -12", bst.contains(-12));
        bst.put(1, 2);
        assertTrue("Checking if contains key 1", bst.contains(1));
        bst.put(1, null);
        assertFalse("Checking if contains key 1", bst.contains(1));
    }

    /** <p>Test {@link BST#height()}.</p> */
    @Test
    public void testHeight() {
        BST<String, Integer> bst = new BST<String, Integer>();
        bst.put("S", 10);
        bst.put("A", 11);
        bst.put("C", 12);
        bst.put("M", 16);
        bst.put("R", 13);
        bst.put("H", 13);
        bst.put("E", 15);
        bst.put("X", 15);
        assertEquals("Checking height of BST", 5, bst.height());
        bst.put("F", 17);
        assertEquals("Checking height of BST", 6, bst.height());
        bst.put("Y", 17);
        assertEquals("Checking height of BST", 6, bst.height());
        bst.put("M", null);
        bst.put("A", null);
        assertEquals("Checking height of BST", 4, bst.height());
    }

    /** <p>Test {@link BST#median()}.</p> */
    @Test
    public void testMedian() {
        BST<String, Integer> bst = new BST<String, Integer>();
        assertNull("Checking median of BST", bst.median());
        bst.put("S", 10);
        bst.put("A", 11);
        bst.put("C", 12);
        bst.put("M", 16);
        bst.put("R", 13);
        bst.put("H", 13);
        bst.put("E", 15);
        bst.put("X", 15);
        assertEquals("Checking median of BST", "H", bst.median());
        bst.put("F", 15);
        assertEquals("Checking median of BST", "H", bst.median());
        bst.put("G", 15);
        assertEquals("Checking median of BST", "G", bst.median());
    }

    // /** <p>Test {@link BST#put(Comparable)}.</p> */
    // @Test
    // public void testPut() {
    //     BST<Integer, Integer> bst = new BST<Integer, Integer>();
    // }

    /** <p>Test {@link BST#printKeysInOrder()}.</p> */
    @Test
    public void testPrintKeysInOrder() {
        BST<String, Integer> bst = new BST<String, Integer>();
        assertEquals("Checking printed keys in order", "()", bst.printKeysInOrder());
        bst.put("S", 10);
        assertEquals("Checking printed keys in order", "(()S())", bst.printKeysInOrder());
        bst.put("A", 11);
        assertEquals("Checking printed keys in order", "((()A())S())", bst.printKeysInOrder());
        bst.put("C", 12);
        assertEquals("Checking printed keys in order", "((()A(()C()))S())", bst.printKeysInOrder());
        bst.put("M", 16);
        assertEquals("Checking printed keys in order", "((()A(()C(()M())))S())", bst.printKeysInOrder());
        bst.put("R", 13);
        assertEquals("Checking printed keys in order", "((()A(()C(()M(()R()))))S())", bst.printKeysInOrder());
        bst.put("H", 13);
        assertEquals("Checking printed keys in order", "((()A(()C((()H())M(()R()))))S())", bst.printKeysInOrder());
        bst.put("E", 15);
        assertEquals("Checking printed keys in order", "((()A(()C(((()E())H())M(()R()))))S())", bst.printKeysInOrder());
        bst.put("X", 15);
        assertEquals("Checking printed keys in order", "((()A(()C(((()E())H())M(()R()))))S(()X()))",
                bst.printKeysInOrder());
    }

    /** <p>Test {@link BST#toString()}.</p> */
    @Test
    public void testToString() {
        BST<String, Integer> bst = new BST<String, Integer>();
        assertEquals("Getting simple in-order string representation of BST", "", bst.toString());
        bst.put("S", 10);
        bst.put("A", 11);
        bst.put("C", 12);
        bst.put("M", 16);
        assertEquals("Getting simple in-order string representation of BST",
                "[\"A\", \"C\", \"M\", \"S\"]", bst.toString());
        bst.put("R", 13);
        bst.put("H", 13);
        bst.put("E", 15);
        bst.put("X", 15);
        assertEquals("Getting simple in-order string representation of BST",
                "[\"A\", \"C\", \"E\", \"H\", \"M\", \"R\", \"S\", \"X\"]", bst.toString());

    }

    /** <p>Test {@link BST#deleteMax()}.</p> */
    @Test
    public void testDeleteMax() {
        BST<String, Integer> bst = new BST<String, Integer>();
        bst.deleteMax();
        assertEquals("Deleting largest key", "", bst.toString());
        bst.put("S", 10);
        bst.deleteMax();
        assertEquals("Deleting largest key", "", bst.toString());
        bst.put("S", 10);
        bst.put("A", 11);
        bst.deleteMax();
        assertEquals("Deleting largest key", "[\"A\"]", bst.toString());
        bst.put("S", 10);
        bst.put("C", 12);
        bst.put("M", 16);
        bst.put("R", 13);
        bst.put("H", 13);
        bst.put("E", 15);
        bst.put("X", 15);
        bst.deleteMax();
        assertEquals("Deleting largest key", 
                "[\"A\", \"C\", \"E\", \"H\", \"M\", \"R\", \"S\"]", bst.toString());
    }

}

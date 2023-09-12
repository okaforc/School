import java.util.ArrayList;

public class App {
    public static void main(String[] args) throws Exception {
        System.out.println("Hello, World!");
        BST<String, Integer> test = new BST<String, Integer>();
        BST<Integer, Integer> bst = new BST<Integer, Integer>();
        // System.out.println(test.median());
        test.deleteMax();
        System.out.println(test.toString());
        ArrayList<String> s = new ArrayList<String>();
        s.add("3");
        s.add("56, 7");
        System.out.println(s.toString());
        bst.put(7, 7);
        bst.put(8, 8);
        bst.put(3, 3);
        bst.put(1, 1);
        bst.put(2, 2);
        bst.put(6, 6);
        bst.put(4, 4);
        bst.put(5, 5);
        System.out.println(bst.prettyPrintKeys());
        bst.delete(9);
        bst.delete(8);
        bst.delete(6);
        bst.delete(3);
        bst.delete(7);
        System.out.println(bst.printKeysInOrder());
        System.out.println(bst.prettyPrintKeys());

        // test.put("S", 10);
        // System.out.println(test.printKeysInOrder());
        // test.put("A", 11);
        // System.out.println(test.printKeysInOrder());
        // test.put("C", 12);
        // System.out.println(test.printKeysInOrder());
        // test.put("M", 16);
        // System.out.println(test.printKeysInOrder());
        // test.put("R", 13);
        // System.out.println(test.printKeysInOrder());
        // test.put("H", 13);
        // System.out.println(test.printKeysInOrder());
        // test.put("E", 15);
        // System.out.println(test.printKeysInOrder());
        // test.put("X", 15);
        // System.out.println(test.printKeysInOrder());
        
        // System.out.println(test.toString());
        // test.put("F", 2);
        // test.put("G", 2);
        
        // System.out.println(test.prettyPrintKeys());
        // System.out.println(test.height());
        // System.out.println(test.median());
        // System.out.println(test.toString());
    }
}

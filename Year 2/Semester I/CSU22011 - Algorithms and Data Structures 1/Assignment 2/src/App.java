import java.util.Arrays;
import java.util.LinkedList;

public class App {
    public static void main(String[] args) throws Exception {
        System.out.println("Hello, World!");

        DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
        System.out.println(testDLL.size());
        testDLL.insertBefore(0, 1);
        System.out.println(testDLL.size());
        testDLL.insertBefore(1, 2);
        testDLL.insertBefore(2, 3);
        testDLL.insertBefore(0, 4);
        testDLL.insertBefore(1, 5);
        testDLL.insertBefore(2, 6);
        testDLL.insertBefore(-1, 7);
        testDLL.insertBefore(7, 8);
        testDLL.insertBefore(4, 10);
        testDLL.insertBefore(4, null);
        System.out.println(testDLL.size());
        System.out.println(testDLL.toString());

        // LinkedList<Integer> ll = new LinkedList<>();
        // ll.add(null);
        // ll.add(1);
        // System.out.println(Arrays.toString(ll.toArray()));
    }
}

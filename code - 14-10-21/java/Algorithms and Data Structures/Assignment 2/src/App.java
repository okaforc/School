public class App {
    public static void main(String[] args) throws Exception {
        System.out.println("Hello, World!");

        DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
        testDLL.insertBefore(0, 1);
        testDLL.insertBefore(1, 2);
        testDLL.insertBefore(2, 3);

        testDLL.insertBefore(0, 4);

        System.out.println(testDLL.toString());
    }
}

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

//-------------------------------------------------------------------------
/**
 * Test class for Doubly Linked List
 *
 * @author Chike Okafor
 * @version 13/10/16 18:15
 */
@RunWith(JUnit4.class)
public class DoublyLinkedListTest {
	// ~ Constructor ........................................................
	@Test
	public void testConstructor() {
		new DoublyLinkedList<Integer>();
	}

	// ~ Public Methods ........................................................

	// ----------------------------------------------------------
	/**
	 * Check if the insertBefore works
	 */
	@Test
	public void testInsertBefore() {
		// test non-empty list
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(0, 1);

		// test list with single node
		testDLL.insertBefore(1, 2);
		assertEquals("Checking insertBefore to a list containing 1 element at position 0", "1,2", testDLL.toString());
		testDLL.insertBefore(-123, 4);
		assertEquals("Checking insertBefore to a list containing 2 elements at position -123", "4,1,2",
				testDLL.toString());
		testDLL.insertBefore(50, 9);
		assertEquals("Checking insertBefore to a list containing 3 elements at position 50", "4,1,2,9",
				testDLL.toString());

		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(0, 1);
		testDLL.insertBefore(1, 2);
		testDLL.insertBefore(2, 3);
		assertEquals("Checking insertBefore to a list containing 2 elements at position 3", "1,2,3",
				testDLL.toString());

		testDLL.insertBefore(0, 4);
		assertEquals("Checking insertBefore to a list containing 3 elements at position 0", "4,1,2,3",
				testDLL.toString());
		testDLL.insertBefore(1, 5);
		assertEquals("Checking insertBefore to a list containing 4 elements at position 1", "4,5,1,2,3",
				testDLL.toString());
		testDLL.insertBefore(2, 6);
		assertEquals("Checking insertBefore to a list containing 5 elements at position 2", "4,5,6,1,2,3",
				testDLL.toString());
		testDLL.insertBefore(-1, 7);
		assertEquals(
				"Checking insertBefore to a list containing 6 elements at position -1 - expected the element at the head of the list",
				"7,4,5,6,1,2,3", testDLL.toString());
		testDLL.insertBefore(7, 8);
		assertEquals(
				"Checking insertBefore to a list containing 7 elemenets at position 8 - expected the element at the tail of the list",
				"7,4,5,6,1,2,3,8", testDLL.toString());
		testDLL.insertBefore(700, 9);
		assertEquals(
				"Checking insertBefore to a list containing 8 elements at position 700 - expected the element at the tail of the list",
				"7,4,5,6,1,2,3,8,9", testDLL.toString());
		testDLL.insertBefore(4, 10);
		assertEquals(
				"Checking insertBefore to a list containing 9 elements at position 4 - expected the element within the list",
				"7,4,5,6,10,1,2,3,8,9", testDLL.toString());
		testDLL.insertBefore(5, null);
		assertEquals("Checking insertBefore to a list containing 9 elements at position 4 - expected no change",
				"7,4,5,6,10,1,2,3,8,9", testDLL.toString());

		// test empty list
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(0, 1);
		assertEquals(
				"Checking insertBefore to an empty list at position 0 - expected the element at the head of the list",
				"1", testDLL.toString());
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(10, 1);
		assertEquals(
				"Checking insertBefore to an empty list at position 10 - expected the element at the head of the list",
				"1", testDLL.toString());
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(-10, 1);
		assertEquals(
				"Checking insertBefore to an empty list at position -10 - expected the element at the head of the list",
				"1", testDLL.toString());
	}

	@Test
	public void testGet() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(0, 14);
		testDLL.insertBefore(1, 25);
		testDLL.insertBefore(2, 36);
		assertEquals("Checking get to a list containing 3 elements at position 0", (Integer) 14, testDLL.get(0));
		testDLL.insertBefore(4, 1010);
		assertNull("Checking get to a list containing 4 elements at position 5", testDLL.get(5));
		assertNull("Checking get to a list containing 4 elements at position -1", testDLL.get(-1));
	}

	@Test
	public void testDeleteAt() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();

		testDLL.deleteAt(0);
		assertEquals("Checking deleteAt to a list containing 0 elements at position 0 - expected empty list", "",
				testDLL.toString());

		testDLL.insertBefore(0, 1);
		testDLL.deleteAt(0);
		assertEquals("Checking deleteAt to a list containing 1 element at position 0 - expected empty list", "",
				testDLL.toString());

		testDLL.insertBefore(0, 1);
		testDLL.insertBefore(1, 2);
		testDLL.insertBefore(2, 3);
		testDLL.insertBefore(3, 4);
		testDLL.insertBefore(4, 5);
		testDLL.insertBefore(5, 6);
		testDLL.insertBefore(6, 7);
		testDLL.insertBefore(7, 8);
		testDLL.insertBefore(8, 9);
		testDLL.insertBefore(9, 10);
		testDLL.insertBefore(10, 11);

		testDLL.deleteAt(420);
		assertEquals("Checking deleteAt to a list containing 11 elements at position 420 - expected no change",
				"1,2,3,4,5,6,7,8,9,10,11", testDLL.toString());
		testDLL.deleteAt(-10000);
		assertEquals("Checking deleteAt to a list containing 11 elements at position -10000 - expected no change",
				"1,2,3,4,5,6,7,8,9,10,11", testDLL.toString());

		assertTrue("Checking deleteAt to a list containing 11 elements at position 10 - expected true",
				testDLL.deleteAt(10));
		assertFalse("Checking deleteAt to a list containing 10 elements at position -10 - expected fasle",
				testDLL.deleteAt(-10));

		testDLL.deleteAt(0);
		assertEquals("Checking deleteAt to a list containing 10 elements at position 0 - expected head to be removed",
				"2,3,4,5,6,7,8,9,10", testDLL.toString());
		testDLL.deleteAt(8);
		assertEquals("Checking deleteAt to a list containing 9 elements at position 0 - expected tail to be removed",
				"2,3,4,5,6,7,8,9", testDLL.toString());
		testDLL.deleteAt(3);
		assertEquals(
				"Checking deleteAt to a list containing 8 elements at position 3 - expected fourth node to be removed",
				"2,3,4,6,7,8,9", testDLL.toString());
	}

	@Test
	public void testReverse() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();

		testDLL.reverse();
		assertEquals("Checking reverse to a list containing 0 elements", "", testDLL.toString());

		testDLL.insertBefore(0, 1);
		testDLL.reverse();
		assertEquals("Checking reverse to a list containing 1 element", "1", testDLL.toString());

		testDLL.insertBefore(1, 2);
		testDLL.insertBefore(2, 3);
		testDLL.insertBefore(3, 4);
		testDLL.insertBefore(4, 5);
		testDLL.insertBefore(5, 6);
		testDLL.insertBefore(6, 7);
		testDLL.insertBefore(7, 8);
		testDLL.insertBefore(8, 9);
		testDLL.insertBefore(9, 10);
		testDLL.insertBefore(10, 11);

		testDLL.reverse();
		assertEquals("Checking reverse to a list containing 11 elements", "11,10,9,8,7,6,5,4,3,2,1",
				testDLL.toString());
	}

	@Test
	public void testMakeUnique() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();

		testDLL.makeUnique();
		assertEquals("Checking makeUnique to a list containing 0 elements", "", testDLL.toString());

		testDLL.insertBefore(0, 1);
		testDLL.makeUnique();
		assertEquals("Checking makeUnique to a list containing 1 element", "1", testDLL.toString());

		testDLL.insertBefore(0, 1);
		testDLL.insertBefore(1, 1);
		testDLL.insertBefore(2, 2);
		testDLL.insertBefore(3, 3);
		testDLL.insertBefore(4, 1);
		testDLL.insertBefore(5, 4);
		testDLL.insertBefore(6, 5);
		testDLL.insertBefore(7, 3);
		testDLL.insertBefore(8, 5);
		testDLL.insertBefore(9, 1);

		testDLL.makeUnique();
		assertEquals("Checking makeUnique to a list containing 10 elements", "1,2,3,4,5", testDLL.toString());
	}

	@Test
	public void testPush() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();

		testDLL.push(0);
		assertEquals("Checking push to a list containing 0 elements", "0", testDLL.toString());
		testDLL.push(1);
		assertEquals("Checking push to a list containing 1 element", "0,1", testDLL.toString());
		testDLL.push(2);
		assertEquals("Checking push to a list containing 2 elements", "0,1,2", testDLL.toString());

	}

	@Test
	public void testPop() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();

		assertNull("Checking pop to a list containing 0 elements", testDLL.pop());

		testDLL.push(0);
		testDLL.pop();
		assertEquals("Checking push to a list containing 1 element", "", testDLL.toString());

		testDLL.push(1);
		testDLL.push(2);
		testDLL.push(3);
		testDLL.push(4);
		testDLL.pop();
		assertEquals("Checking push to a list containing 4 elements", "1,2,3", testDLL.toString());
	}

	@Test
	public void testEnqueue() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();

		testDLL.enqueue(0);
		assertEquals("Checking enqueue to a list containing 0 elements", "0", testDLL.toString());
		testDLL.enqueue(1);
		assertEquals("Checking enqueue to a list containing 1 element", "0,1", testDLL.toString());
		testDLL.enqueue(2);
		assertEquals("Checking enqueue to a list containing 2 elements", "0,1,2", testDLL.toString());

	}

	@Test
	public void testDequeue() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();

		assertNull("Checking dequeue to a list containing 0 elements", testDLL.dequeue());

		testDLL.enqueue(0);
		testDLL.dequeue();
		assertEquals("Checking dequeue to a list containing 1 element", "", testDLL.toString());

		testDLL.enqueue(1);
		testDLL.enqueue(2);
		testDLL.enqueue(3);
		testDLL.enqueue(4);
		testDLL.dequeue();
		assertEquals("Checking dequeue to a list containing 4 elements", "2,3,4", testDLL.toString());
	}

	@Test
	public void testSize() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();

		assertEquals("Checking size of a list containing 0 elements", 0, testDLL.size());

		testDLL.push(1);
		assertEquals("Checking size of a list containing 1 element", 1, testDLL.size());

		testDLL.push(2);
		testDLL.push(3);
		testDLL.push(4);
		testDLL.push(5);
		testDLL.push(6);
		testDLL.push(7);
		assertEquals("Checking size of a list containing 7 element", 7, testDLL.size());

	}

	@Test
	public void testIsEmpty() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();

		assertTrue("Checking emptiness of a list containing 0 elements", testDLL.isEmpty());

		testDLL.push(1);
		assertFalse("Checking emptiness of a list containing 1 element", testDLL.isEmpty());

		testDLL.push(2);
		testDLL.push(3);
		testDLL.push(4);
		testDLL.push(5);
		testDLL.push(6);
		testDLL.push(7);
		assertFalse("Checking emptiness of a list containing 7 element", testDLL.isEmpty());

	}

	@Test
	public void testToString() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();

		assertEquals("Printing list with 0 elements", "", testDLL.toString());

		testDLL.push(1);
		assertEquals("Printing list with 1 element", "1", testDLL.toString());

		testDLL.push(2);
		testDLL.push(3);
		testDLL.push(4);
		testDLL.push(5);
		testDLL.push(6);
		testDLL.push(7);
		assertEquals("Printing list with 7 elements", "1,2,3,4,5,6,7", testDLL.toString());
	}

}

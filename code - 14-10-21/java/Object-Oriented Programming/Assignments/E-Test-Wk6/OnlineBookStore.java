import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

public class OnlineBookStore {
	public static int ISBN_INDEX = 0;
	public static int TITLE_INDEX = 1;
	public static int AUTHOR_INDEX = 2;
	public static int PUBLISHER_INDEX = 3;
	public static int PUBLISHER_YEAR_INDEX = 4;
	public static int QUANTITY_INDEX = 5;
	public static int PRICE_INDEX = 6;

	public static void main(String[] args) {
		ArrayList<Book> bookList = new ArrayList<Book>();
		try {
			FileReader fileReader = new FileReader("books.txt");// Enter the entire path of the file if needed
			BufferedReader bufferedReader = new BufferedReader(fileReader);
			boolean endOfFile = false;

			while (!endOfFile) {
				String bookLine = bufferedReader.readLine();
				if (bookLine != null) {
					String[] bookData = bookLine.split(", ");
					String isbn = bookData[ISBN_INDEX];
					String title = bookData[TITLE_INDEX];
					String author = bookData[AUTHOR_INDEX];
					String publisher = bookData[PUBLISHER_INDEX];
					int publishYear = Integer.parseInt(bookData[PUBLISHER_YEAR_INDEX]);
					int quantity = Integer.parseInt(bookData[QUANTITY_INDEX]);
					double price = Double.parseDouble(bookData[PRICE_INDEX]);
					Book book = new Book(isbn, title, author, publisher, publishYear, quantity, price);
					bookList.add(book);

				} else {
					endOfFile = true;
				}
			}
			bufferedReader.close();
			fileReader.close();
		} // End try

		catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

		// Uncomment the following lines once you have implemented the required methods
		printBookDetails(bookList);
		purchaseBook(bookList);
	}

	public static void printBookDetails(ArrayList<Book> bookList) {
		try {
			BufferedReader br = new BufferedReader(new FileReader("books.txt"));
			int i;
			while ((i = br.read()) != -1) {
				System.out.print((char)i);
			}
			System.out.println("\n");
			br.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static Book getBook(ArrayList<Book> bookList, String title) {
		for (Book book : bookList) {
			try {
				if (title.equalsIgnoreCase(book.getTitle())) {
					return book;
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		return null;
	}

	public static void topUpCard(ChargeCard card, double amount) {
		card.topUpFunds(amount);
	}

	public static void purchaseBook(ArrayList<Book> bookList) {
		Scanner sc = new Scanner(System.in);
		String funds;
		while (true) {
			try {
				System.out.print("How much money do you have? ");
				funds = sc.nextLine();
				if (funds.equalsIgnoreCase("quit")) {
					return;
				}
				if (Double.parseDouble(funds) < 0) {
					throw new Exception("You can't have negative money!");
				} else {
					break;
				}

			} catch (Exception e) {
				System.out.println("Error! Please try again.");
				System.out.println("Reason: " + e);
			}
		}

		double userMoney = Double.parseDouble(funds);
		while (true) {
			try {
				System.out.print("What book would you like? ");
				String userBook = sc.nextLine();
				if (userBook.equalsIgnoreCase("quit")) {
					break;
				}

				for (Book book : bookList) {
					if (userBook.equalsIgnoreCase(book.getTitle())) {
						if (userMoney - book.getPrice() < 0) {
							System.out.println("You can't afford this.\n");
							break;
						}

						if (book.getQuantity() <= 0) {
							System.out.println("Sorry, that title is out of stock.\n");
							break;
						} else {
							book.setQuantity(book.getQuantity() - 1);
							userMoney -= book.getPrice();
							System.out.printf("You have purchased \"%s\". You have $%.2f left.\n", book.getTitle(),
									userMoney);
							break;
						}
					}
				}
			} catch (Exception e) {
				System.out.println("Error! Please try again.");
				System.out.println("Reason: " + e);
			}
		}
		sc.close();
	}

}
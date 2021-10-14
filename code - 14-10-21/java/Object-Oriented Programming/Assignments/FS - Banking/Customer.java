import java.util.*;

public class Customer {

    String owner_a, owner_b;
    String address;
    ArrayList<String> numbers = new ArrayList<>();
    ArrayList<Account> accounts = new ArrayList<>();

    public Customer(String name, String address, ArrayList<String> numbers) {
        owner_a = name;
        this.address = address;
        this.numbers = numbers;
    }

    public void createAccount(int id, double balance, ArrayList<Transaction> transactions, String type) {
        accounts.add(new Account(this, id, balance, transactions, type));
    }
}

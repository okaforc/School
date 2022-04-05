import java.util.*;

public class Bank {
    public Bank() {};
    ArrayList<Customer> customers = new ArrayList<>();
    ArrayList<Integer> UAN = new ArrayList<>();     // list of account numbers. must all be unique

    public void createCustomer(String name, String address, ArrayList<String> numbers) {
        customers.add(new Customer(name, address, numbers));
    }
}

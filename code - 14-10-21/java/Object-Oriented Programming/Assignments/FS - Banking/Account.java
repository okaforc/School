import java.util.*;

public class Account {
    Customer owner_a, owner_b;
    double balance;
    long id;
    boolean joint;
    String accType;
    ArrayList<Transaction> transactions = new ArrayList<>();

    public Account(Customer cust, long id, double balance, ArrayList<Transaction> transactions, String accType) {
        this.balance = balance;
        this.id = id;
        this.joint = false;
        this.transactions = transactions;
        this.accType = accType;
        owner_a = cust;
    }
    
    public Account(Customer cust_a, Customer cust_b, long id, double balance, ArrayList<Transaction> transactions, String accType) {
        this.balance = balance;
        this.id = id;
        this.joint = true;
        this.transactions = transactions;
        this.accType = accType;
        owner_a = cust_a;
        owner_b = cust_b;
    }

    public void createTransaction(String date, double amount, String desc) {
        transactions.add(new Transaction(date, amount, desc));
    }

    public void addCustomer(Customer cust) {
        if (owner_b == null) {
            owner_b = cust; 
            joint = true;
        } else {
            throw new Error("This account already has two owners.");
        }
    }

    void chooseAccountType(String type) {
        switch (type) {
            case "current", "savings", "credit", "mortgage":
                accType = type;
                break;
        }
    }
}

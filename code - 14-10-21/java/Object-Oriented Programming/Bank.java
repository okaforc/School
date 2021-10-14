import java.util.ArrayList;

public class Bank {

    public static void main(String[] args) {
        BankCustomer bc = new BankCustomer();
        bc.setAccountNumber(1102);
        bc.setCustomerAddress("that one place in canada");
        bc.setCustomerDOB("a day this year, probably");
        bc.setCustomerName("Iris");
        // System.out.println(bc.getCustomerName());
        BankCustomer bc2 = new BankCustomer();
        bc2.setAccountNumber(6626);
        bc2.setCustomerAddress("spider land");
        bc2.setCustomerDOB("septembruary");
        bc2.setCustomerName("Lucina");
        
        ArrayList<BankCustomer> bcArr = new ArrayList<BankCustomer>();
        bcArr.add(bc);
        System.out.println(BankCustomer.findCustomer(10, bcArr));
        bcArr.add(bc2);
        System.out.println(BankCustomer.findCustomer(10, bcArr));

    }

}

import java.util.ArrayList;

public class BankCustomer {
    private int accountNumber;
    private String customerName, customerAddress, customerDateOfBirth;
    public static void main(String[] args) {
        
    }
    
    public String getCustomerName() {
        return this.customerName;
    }
    
    public int getAccountNumber() {
        return this.accountNumber;
    }
    
    public String getCustomerDOB() {
        return this.customerDateOfBirth;
    }
    
    public String getCustomerAddress() {
        return this.customerAddress;
    }
    
    public void setCustomerName(String custName) {
        this.customerName = custName;
    }

    public void setCustomerAddress(String custAdd) {
        this.customerAddress = custAdd;
    }

    public void setAccountNumber(int accNum) {
        this.accountNumber = accNum;
    }

    public void setCustomerDOB(String custDOB) {
        this.customerDateOfBirth = custDOB;
    }

    public static String findCustomer(int num, ArrayList<BankCustomer> arr) {
        return arr.get(0).getCustomerName();
    }

    public static String findCustomer(String dob, String address, BankCustomer[] arr) {
        return arr[0].getCustomerName();
    }
}

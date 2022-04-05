public class Currency implements Portfolio {
    double amount;

    public Currency(double amount) {
        this.amount = amount;
    }

    @Override
    public double marketVal() {
        return amount;
    }

    @Override
    public double profit() {
        return 0;
    }

    public String toString() {
        return "Cash ( $ " + String.format("%.2f", amount) + " )";
    }
    
}

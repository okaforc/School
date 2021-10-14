public abstract class MyAssets implements Portfolio {
    String symbol;
    double totalCost;
    double curPrice;
    double diff;

    public abstract double marketVal();

    @Override
    public double profit() {
        return diff;
    }
    
}

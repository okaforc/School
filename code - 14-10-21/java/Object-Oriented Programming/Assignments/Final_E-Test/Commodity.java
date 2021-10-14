public class Commodity extends MyAssets {
    int totalShares;
    double initCost;
    String symbol;

    public Commodity(String symbol, double curPrice) {
        this.symbol = symbol;
        this.curPrice = curPrice;
    }
    
    public void purchase(int amnt, double price) {
        totalShares = amnt;
        totalCost = price * amnt;
        initCost = totalCost;
    }

    public void setCurrentPrice(double price) {
        this.curPrice = price;
        diff = (totalShares * price) - totalCost;
        totalCost = totalShares * price;
    }

    @Override
    public double marketVal() {
        return totalCost;
    }

    public String toString() {
        return symbol + " ( " + totalShares + " shares, $ " + String.format("%.2f", initCost) + " total cost )";
    }
}

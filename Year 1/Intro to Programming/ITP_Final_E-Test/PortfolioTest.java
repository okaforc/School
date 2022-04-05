public class PortfolioTest {
    public static void main(String[] args) {
        Portfolio[] investments = new Portfolio[4];
        investments[0] = new Commodity("CITI", 30.00);
        investments[1] = new Currency(500.00);
        investments[2] = new Currency(1200.54);
        investments[3] = new Commodity("DAA", 25.00);

        ((Commodity) investments[0]).purchase(10, 30.00);
        ((Commodity) investments[0]).setCurrentPrice(27.50);

        ((Commodity) investments[3]).purchase(30, 20.00);
        ((Commodity) investments[3]).setCurrentPrice(23.00);

        for (int i = 0; i < investments.length; i++) {
            System.out.println(investments[i]);
            System.out.println(
                    "Market value is: " + investments[i].marketVal() + ", profit is: " + investments[i].profit());
            System.out.println();
        }
    }
}

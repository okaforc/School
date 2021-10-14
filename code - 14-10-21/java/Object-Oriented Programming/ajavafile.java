public class ajavafile {
    public static void main(String[] args) {
        System.out.println(Innerajavafile.test);
    }
}

class Innerajavafile {
    static String test = "This apparently works.";
    
}





/* import java.util.Random;

public class Dice {
    Random rand = new Random();
    private int result = rand.nextInt(6) + 1; // random number from 1 to 6 inclusive

    public int getResult() {
        return result;
    }
} 


public class Wallet {
    private double money;

    public double getMoney() {
        return money;
    }

    public void setMoney(double money) {
        this.money = money;
    }
    
}


*/

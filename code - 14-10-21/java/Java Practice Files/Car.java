public class Car {
    public void fullThrottle() {
        System.out.println("Max speed.");
    }

    public void speed(int maxSpeed) {
        System.out.println("Max speed is: " + maxSpeed);
    }

    public static void main(String[] args) {
        Car myCar = new Car();
        myCar.fullThrottle();
        myCar.speed(200);
    }
}

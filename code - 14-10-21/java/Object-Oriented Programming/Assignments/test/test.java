public class test {
    public static void main(String[] args) {
        Shape s1 = new Square("blue", 10);
        System.out.printf("The perimeter of this square is %.2f\n", s1.getPerimeter());
        System.out.printf("The area of this square is %.2f\n\n", s1.getArea());
        Shape s2 = new Triangle("red", 9, 5);
        System.out.printf("The perimeter of this triangle is %.2f\n", s2.getPerimeter());
        System.out.printf("The area of this triangle is %.2f", s2.getArea());
        
    }
}

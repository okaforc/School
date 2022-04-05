public class LineTest {
    public static void main(String[] args) {
        Point p1 = new Point(4, 18);
        Point p2 = new Point(10, 8);
        Line line_1 = new Line(p1, p2);
        System.out.printf("Line 1: (%d, %d), (%d, %d)\n", line_1.getP1().getX(), line_1.getP1().getY(),
                line_1.getP2().getX(), line_1.getP2().getY());
        System.out.printf("Slope: %.2f\n\n", line_1.getSlope());

        Line line_2 = new Line(6, 11, 9, 20);
        System.out.printf("Line 2: (%d, %d), (%d, %d)\n", line_2.getP1().getX(), line_2.getP1().getY(),
                line_2.getP2().getX(), line_2.getP2().getY());
        System.out.printf("Slope: %.2f", line_2.getSlope());
    }
}

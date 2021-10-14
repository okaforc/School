public class Line {
    private Point p1;
    private Point p2;

    // Create a line using 2 given points
    public Line(Point p1, Point p2) {
        this.p1 = p1;
        this.p2 = p2;
    }

    // Create a line using 4 given coordinates
    public Line(int x1, int y1, int x2, int y2) {
        this.p1 = new Point(x1, y1);
        this.p2 = new Point(x2, y2);
    }

    // Return first point of this line
    public Point getP1() {
        return p1;
    }
    
    // Return second point of this line
    public Point getP2() {
        return p2;
    }

    // Return the slope of the line
    public double getSlope() {
        double ans = 0;
        int divTop = p2.getY() - p1.getY();
        int divBottom = p2.getX() - p1.getX();
        try {
            ans = divTop / (double)divBottom;
            if (divBottom == 0 || p2.getX() == p1.getX()) {
                // checking for either case, as doubles are sometimes inaccurate
                throw new ArithmeticException("You can't divide by 0 and your x-coordinates cannot be equal. Your slope is a vertical line.");
            }
        } catch (Exception e) {
            System.out.println(e);
        }

        return ans;
    }
}

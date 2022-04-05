public class Point 
{
    private int x;
    private int y;

    // Constructs a new point at the origin, (0, 0).
    public Point()
    {
        x = 0;
        y = 0;
    }

    // Constructs a new point with the given (new_x, new_y) location.
    // pre: new_x >= 0 && new_y >= 0
    public Point(int new_x, int new_y)
    {
        if (new_x < 0 || new_y < 0) {
            throw new IllegalArgumentException();
    }

        x = new_x;
        y = new_y;
    }

    // Returns the x-coordinate of this point.
    public int getX()
    {
        return x;
    }

    // Returns the y-coordinate of this point.
    public int getY()
    {
        return y;
    }
}
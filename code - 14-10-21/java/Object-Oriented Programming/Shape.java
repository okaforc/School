/**
 * Shape
 */
public interface Shape {

    double getArea();
}

/**
 * Rectangle
 */
class Rectangle implements Shape {

    private double length;
    private double width;

    @Override
    public double getArea() {
        return length * width;
    }

    
}
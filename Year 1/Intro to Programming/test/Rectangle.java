public abstract class Rectangle implements Shape {
    private String color;
    private int length;
    private int width;
    
    public Rectangle(String color, int length, int width) {
        this.length = length;
        this.color = color;
        this.width = width;
    }

    @Override
    public double getArea() {
        return length * width;
    }

    @Override
    public abstract double getPerimeter();


}

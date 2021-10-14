/**
 * Cube
 */
public class Shapes {
    public static void main(String[] args) {
        
        Box myBox1 = new Box();
        Box myBox2 = new Box(1, 3, 5);
        Box cube = new Box(7);
    
        double vol = cube.volume();
        double vol2 = myBox1.volume();
        double vol3 = myBox2.volume();
        System.out.println(vol + " m^3");
        System.out.println(vol2 + " m^3");
        System.out.println(vol3 + " m^3");
    }
    
}

class Box {
    private double depth, height, width;

    Box() {
        depth = height = width = 0;
    }

    Box(double d, double h, double w) {
        width = w;
        height = h;
        depth = d;
    }

    Box (double d) {
        depth = height = width = d;
    }

    public double volume() {
        return depth * height * width;
    }
}














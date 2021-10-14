public class ComplexDemo {
    public static void main(String[] args) {
        Complex cm1 = new Complex(3, 1);
        Complex cm2 = new Complex(4, 2);

        Complex.add(cm1, cm2);
        System.out.println(cm2.toString());
    }

}

class Complex {
    double x, y;

    Complex() {
        x = 0;
        y = 0;
    }

    Complex(double r, double i) {
        x = r;
        y = i;
    }

    public static Complex add(Complex a, Complex b) {
        // a = new Complex();
        // b = new Complex
        return new Complex(a.x + b.x, a.y + b.y);
    }

    public static Complex multiply(Complex a, Complex b) {
        return new Complex((a.x * b.x - a.y * b.y), (a.x * b.y + a.y * b.x));
    }

    public static Complex divide(Complex a, Complex b) {
        double temp1 = (a.x*b.x - a.y*b.y);
        double temp2 = (b.x * a.y + b.y * a.x);
        // double temp3 = temp1 + temp2;
        double temp3 = Math.pow(b.x, 2) + Math.pow(b.y, 2);
        return new Complex(temp1/temp3, temp2/temp3);
    }

    public String toString() {
        return x + ", " + y;
    }
}
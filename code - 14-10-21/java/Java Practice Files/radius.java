import java.util.Scanner;

public class radius {
	public static void main(String[] args) {
		Scanner scan = new Scanner(System.in);
		double radius;
		double diameter;
		double circumference;
		double area;

		System.out.print("Radius: ");
		radius = scan.nextDouble();
		// if(scan.hasNext()) {
		// System.out.println("qwerty");
		// }
		scan.close();

		diameter = radius * 2;
		circumference = diameter * Math.PI;
		area = Math.pow(radius, 2) * Math.PI;

		System.out.printf("Given a circle of radius %f:\n\tarea = %.3f, \n\tdiameter = %f, \n\tcircumference = %f.",
				radius, area, diameter, circumference);

	}
}

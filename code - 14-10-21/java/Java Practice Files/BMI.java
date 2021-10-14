import java.util.Scanner;

public class BMI {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        float weight;
        float height;
        double bmi;

        // bmi = weight/height^2

        while (true) {
            try {
                System.out.print("What is your weight in kg? ");
                weight = sc.nextFloat();
                System.out.print("What is your height in metres? ");
                height = sc.nextFloat();
                sc.close();

                bmi = weight / (Math.pow(height, 2));

                System.out.printf("Your BMI is %f", bmi);
                System.out.printf(
                        "\nA BMI is %.1f means the person is either " + ((bmi>=25) ? "overweight" : "normal") + ".\n",
                        bmi);
                break;
            } catch (Exception e) {
                System.out.println("failed");
                break;
            }
        }

    }
}

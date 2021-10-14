import java.util.Scanner;

public class Tutorial_Week3 {

	public static void main(String[] args) {
        /* 
        for moeto s.
        */



		Scanner input = new Scanner (System.in);
		System.out.print("Write 3 numbers for computing their average and standard deviation(number1,number2,number3): ");
        String[] nums = input.nextLine().trim().split(",");
        int sum = 0;
        input.close();
        
        for (String string : nums) {
            string = string.trim();
            sum += Integer.parseInt(string);
        }
		
		double average = (double) (sum / 3);
		
		System.out.print("Average is" + average);
		
	}

}
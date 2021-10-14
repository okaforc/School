import java.util.*;



public class addRoots {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int userNum;
        List<String> numbers = new ArrayList<String>();

        System.out.print("Enter number: ");
        userNum = scan.nextInt();
        scan.close();

        // System.out.println("The numbers whose squares are less than or equal to 27 are ");
        try {
            if(userNum > 1) {
                for (int i = 0; i <= userNum; i++) {
                    if(i*i <= userNum) {
                        numbers.add(String.valueOf(i));
                    }
                }
            } else {
                if(userNum == 0) {
                    System.out.println("Inputting 0 will always return only 0. Please choose a larger number.");
                } else if (userNum < 0) {
                    System.out.printf((String.format("There are no squares less than or equal to %d.", userNum)));
                } 
            }
        } catch (Throwable e) {
            System.out.println("ssss");
        }
        
        String nums = numbers.get(0);

        for (int i = 1; i < numbers.size(); i++) {
            nums += ", " + numbers.get(i);
        }

        System.out.printf(String.format("The numbers whose squares are less than or equal to %d are ", userNum) + nums + ".");
    }
}

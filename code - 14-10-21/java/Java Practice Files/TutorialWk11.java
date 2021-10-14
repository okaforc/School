import java.util.*;

public class TutorialWk11 {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        ArrayList<Integer> rawNums = new ArrayList<Integer>();
        while (true) {
            try {
                System.out.print("Enter a number: ");
                String rawUserNum = sc.next();
                if (rawUserNum.equalsIgnoreCase("quit")) {
                    break;
                }
                int userNum = Integer.parseInt(rawUserNum);
                rawNums.add(userNum);
                int[] userArray = new int[rawNums.size()];

                for (int i = 0; i < userArray.length; i++) {
                    userArray[i] = rawNums.get(i);
                }
                char[] numChar = String.valueOf(userNum).toCharArray();
                countDigitFrequencies(userArray, userNum);
                for (char c : numChar) {
                    System.out.print(c);
                    printDigitFrequencies(userArray);
                }
                System.out.println("\n");
            } catch (Exception e) {
            }
        }

        sc.close();
    }

    public static void printDigitFrequencies(int[] frequencies) {
        for (int i : frequencies) {
            System.out.printf("(%d) ", i);
        }
        // System.out.println("\n");
    }

    public static void countDigitFrequencies(int[] frequencies, int number) {
        char[] numChar = String.valueOf(number).toCharArray();
        for (int i = 0; i < frequencies.length; i++) {
            for (int j = i+1; j < frequencies.length; j++) {
                if (numChar[i] == numChar[j]) {
                    frequencies[i]++;
                }
            }
        }
    }

}

import java.util.*;
import java.util.stream.IntStream;

/* SELF ASSESSMENT 
   1.    createSequence:
      Did I use the correct method definition?
      Mark out of 5: 5
      Comment: I used the correct method definition
      Did I create an array of size n (passed as the parameter) and initialise it?
      Mark out of 5: 5
      Comment: I created and initialised an array of size parameter n
      Did I return the correct item?
      Mark out of 5: 5
      Comment: I returned a String array

   2.    crossOutMultiples
      Did I use the correct method definition?
      Mark out of 5:5
      Comment:I used the correct method definition
      Did I ensure the parameters are not null and one of them is a valid index into the array
      Mark out of 2:2
      Comment:I ensured the parameters are correct
      Did I loop through the array using the correct multiple?
      Mark out of 5:5
      Comment:I looped through the array correctly
      Did I cross out correct items in the array that were not already crossed out?
      Mark out of 3:3
      Comment: I crossed out only non-crossed out numbers

   3.    sieve   
      Did I have the correct function definition?
      Mark out of 5:5
      Comment:I used the correct method definition
      Did I make calls to other methods?
      Mark out of 5:5
      Comment:I made calls to other methods
      Did I return an array with all non-prime numbers are crossed out?
      Mark out of 2:2
      Comment:I returned the correct array

   4.    sequenceTostring  
      Did I have the correct function definition?
      Mark out of 5:5
      Comment:I used the correct method definition
      Did I ensure the parameter to be used is not null?
      Mark out of 3:3
      Comment: I checked if the array is null
      Did I Loop through the array updating the String variable with the non-crossed out numbers and the crossed numbers in brackets? 
      Mark out of 10:10
      Comment:I updated the string

   5.    nonCrossedOutSubseqToString  
      Did I have the correct function definition
      Mark out of 5:5
      Comment:I used the correct method definition
      Did I ensure the parameter to be used is not null?
      Mark out of 3:3
      Comment:I checked if the array is null
      Did I loop through the array updating the String variable with just the non-crossed out numbers? 
      Mark out of 5:5
      Comment:I updated the string

   6.    main  
      Did I ask  the user for input n and handles input errors?  
      Mark out of 5:5
      Comments: The user is asked for an integer, and the program handles input errors
      Did I make calls to other methods (at least one)?
      Mark out of 5:5
      Comment:  I made calls to the other methods
      Did I print the output as shown in the question?  
      Mark out of 5:5
      Comment:  I followed the example output

   7.    Overall
      Is my code indented correctly?
      Mark out of 4:4
      Comments: my code is indented properly
      Do my variable names make sense?
      Mark out of 4:4
      Comments: my variable names are understandable
      Do my variable names, method names and class name follow the Java coding standard
      Mark out of 4:4
      Comments: I used camelCase to name them

   Total Mark out of 100 (Add all the previous marks): 100
*/

/**
 * SieveOfEratosthenes
 */
public class SieveOfEratosthenes {
   final static String CROSS_OUT_START = "[";
   final static String CROSS_OUT_END = "]";

   public static void main(String[] args) {
      Scanner input = new Scanner(System.in);
      System.out.print("Enter int >= 2 : ");
      try {
         int userNum = input.nextInt();
         System.out.println(sequenceToString(createSequence(userNum)));
         System.out.println(nonCrossedOutSubseqToString(sieve(userNum)));
      } catch (Exception e) {
         System.out.println("You must enter a positive integer greater than or equal to 2.");
      }
      input.close();
   }

   public static String[] createSequence(int n) {
      if (n >= 2) {
         int[] rawNums = IntStream.range(2, n + 1).toArray();  // create an int[] with values ranging from 2 to n
         String[] nums = new String[rawNums.length];
         for (int i = 0; i < nums.length; i++) {
            nums[i] = String.valueOf(rawNums[i]);
         }
         return nums;
      }
      String[] empty = {};
      return empty;
      
   }

   public static String[] crossOutHigherMultiples(String[] nums, int n) {
      if (nums != null && n >= 2) {
         String[] newNums = new String[nums.length];
         System.arraycopy(nums, 0, newNums, 0, nums.length);
         // if any value has a multiple and isn't already crossed out, cross it out
         for (int i = 0; i < Math.sqrt(n); i++) {
            if (!newNums[i].contains("[")) {
               for (int j = i + 1; j < n - 1; j++) {
                  if (Integer.parseInt(nums[j]) % Integer.parseInt(nums[i]) == 0 && !nums[j].contains("[")) {
                     // aka "[" + number.toString() + "]"
                     newNums[j] = CROSS_OUT_START + String.valueOf(nums[j]) + CROSS_OUT_END;
                  }
               }
            }
            if (!newNums[i].contains("[") && Integer.parseInt(newNums[i]) <= Math.sqrt(n)) {
               System.out.println(sequenceToString(newNums));
            }
         }
         return newNums;
      }
      String[] empty = {};
      return empty;
   }

   public static String[] sieve(int n) {
         return crossOutHigherMultiples(createSequence(n), n);
      
   }

   public static String sequenceToString(String[] nums) {
      if (nums != null) {
         String sequence = "";
         for (String n : nums) {
            sequence += n + ", ";
         }
         return sequence.substring(0, sequence.length() - 2); // remove final ", "
      }
      return "";
   }

   public static String nonCrossedOutSubseqToString(String[] nums) {
      if (nums != null) {
         String nonCrossed = "";
         for (int i = 0; i < nums.length; i++) {
            if (!nums[i].contains("[")) {
               nonCrossed += nums[i] + ", ";
            }
         }
         return nonCrossed.substring(0, nonCrossed.length() - 2);
      }
      return "";
   }
}
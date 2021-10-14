/* SELF ASSESSMENT 

1. readDictionary
- I have the correct method definition [Mark out of 5:5]
- Comment: My method definition returns an ArrayList of strings and throws an IOException
- My method reads the words from the "words.txt" file. [Mark out of 5:5]
- Comment: I used the BufferedReader and FileReader libraries to read the text file
- It returns the contents from "words.txt" in a String array or an ArrayList. [Mark out of 5:5]
- Comment: The method returns each line from the text file as an element in the ArrayList

2. readWordList
- I have the correct method definition [Mark out of 5:5]
- Comment: My method definition returns an ArrayList of strings
- My method reads the words provided (which are separated by commas, saves them to an array or ArrayList of String references and returns it. [Mark out of 5:5]
- Comment: My method return the words provided as an ArrayList of strings

3. isUniqueList
- I have the correct method definition [Mark out of 5:5]
- Comment: My method definition takes in an ArrayList of strings and returns a boolean
- My method compares each word in the array with the rest of the words in the list. [Mark out of 5:5]
- Comment: I used two for loops to compare each word in the list with every other word
- Exits the loop when a non-unique word is found. [Mark out of 5:5]
- Comment: The program exits its loop and return false if any word is the same as another
- Returns true is all the words are unique and false otherwise. [Mark out of 5:5]
- Comment: The method compares all values and returns its result

4. isEnglishWord
- I have the correct method definition [Mark out of 5:5]
- Comment: My method definition takes in a string and returns a boolean
- My method uses the binarySearch method in Arrays library class. [Mark out of 3:3]
- Comment: I used Arrays.binarySearch to find if the word was in the list
- Returns true if the binarySearch method return a value >= 0, otherwise false is returned. [Mark out of 2:2]
- Comment: I returned the boolean of whether or not the string was in the text file

5. isDifferentByOne
- I have the correct method definition [Mark out of 5:5]
- Comment: My method definition takes in two strings and returns a boolean
- My method loops through the length of a words comparing characters at the same position in both words searching for one difference. [Mark out of 10:10]
- Comment: I set up an integer and compared the characters of each string. If two any characters were different, I updated a counter. If this counter was equal to exactly 1, I returned true. Otherwise, I returned false.

6. isWordChain
- I have the correct method definition [Mark out of 5:5]
- Comment: My method definition takes in an ArrayList of strings and returns a boolean
- My method calls isUniqueList, isEnglishWord and isDifferentByOne methods and prints the appropriate message [Mark out of 10:10]
- Comment: I used all method mentioned above

7. main
- Reads all the words from file words.txt into an array or an ArrayList using the any of teh Java.IO classes covered in lectures [Mark out of 10:10]
- Comment: I used readDictionary to read each word and put them in an ArrayList
- Asks the user for input and calls isWordChain [Mark out of 5:5]
- Comment: I use the Scanner class to ask for input and use isWordChain with readWordList to execute the main part of the program

 Total Mark out of 100 (Add all the previous marks):100
*/

import java.io.*;
import java.util.*;

/**
 * WordLinks
 */
public class WordLinks {
    static ArrayList<String> wordArray = new ArrayList<String>();
    static Scanner input = new Scanner(System.in);
    static boolean hasQuit = false;
    static ArrayList<String> ALDict;
    static String[] dict;
    static String userWords;

    public static void main(String[] args) {
        try {
            ALDict = readDictionary();
            dict = new String[ALDict.size()];
            dict = ALDict.toArray(dict);
            Arrays.sort(dict);
        } catch (Exception e) {
            e.printStackTrace();
        }

        while (true) {
            try {
                if (hasQuit == true) {
                    break;
                }
                System.out.print("Enter a comma separated list of words (or an empty list to quit): ");
                userWords = input.nextLine();
                isWordChain(readWordList());

            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        input.close();

    }

    public static ArrayList<String> readDictionary() throws IOException {
        ArrayList<String> allWords = new ArrayList<String>();
        BufferedReader br = new BufferedReader(new FileReader("words.txt"));
        String i;
        while ((i = br.readLine()) != null) {
            allWords.add(i.toLowerCase());
        }
        br.close();
        return allWords;
    }

    public static ArrayList<String> readWordList() {
        String[] tempArr;
        if (userWords.isEmpty() || userWords.isBlank()) {
            hasQuit = true;
        }
        
            tempArr = userWords.replace(" ", "").split(",");
        

        wordArray = new ArrayList<String>(Arrays.asList(tempArr));
        return wordArray;
    }

    public static boolean isUniqueList(ArrayList<String> words) {
        for (int i = 0; i < words.size(); i++) {
            String cmp1 = words.get(i);
            for (int j = 0; j < words.size(); j++) {
                String cmp2 = words.get(j);
                if (i == j)
                    continue;
                if (cmp1.equalsIgnoreCase(cmp2)) {
                    return false;
                }
            }
        }
        return true;
    }

    public static boolean isEnglishWord(String str) {
        return Arrays.binarySearch(dict, str.toLowerCase()) >= 0;
    }

    public static boolean isDifferentByOne(String a, String b) {
        int count = 0;
        if (a.length() != b.length())
            return false;
        for (int i = 0; i < a.length(); i++) {
            if (a.toCharArray()[i] != b.toCharArray()[i]) {
                count++;
            }
        }
        return count == 1 ? true : false;
    }

    public static boolean isWordChain(ArrayList<String> words) throws IOException {
        if (hasQuit) {
            return true;
        }
        if (words.size() < 2) {
            System.out.println("Not a valid chain of words from Lewis Carroll's word-links game.\n");
            return false;
        }
        String a = "", b = "";
        if (isUniqueList(words)) {
            for (int i = 1; i < words.size(); i++) {
                if (isEnglishWord(words.get(i).toLowerCase())) {
                    a = words.get(i);
                } else {
                    System.out.println("Not a valid chain of words from Lewis Carroll's word-links game.\n");
                    return false;
                }
                if (isEnglishWord(words.get(i - 1).toLowerCase())) {
                    b = words.get(i - 1);
                } else {
                    System.out.println("Not a valid chain of words from Lewis Carroll's word-links game.\n");
                    return false;
                }
                
                if (!isDifferentByOne(a, b)) {
                    System.out.println("Not a valid chain of words from Lewis Carroll's word-links game.\n");
                    return false;
                }
            }
        }
        System.out.println("Valid chain of words from Lewis Carroll's word-links game.\n");
        return true;
    }
}
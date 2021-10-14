import java.util.ArrayList;

public class validParen {
    public static void main(String[] args) {
        System.out.println(isValid("(){}[])"));
    }

    public static boolean isValid(String s) {
        ArrayList<Character> stack = new ArrayList<>();
        // oS, oC, oQ
        char oS = '(', cS = ')', oC = '{', cC = '}', oQ = '[', cQ = ']';

        for (Character c : s.toCharArray()) {
            stack.add(c);
            if (c.equals(cS) && stack.contains(oS)) {
                stack.remove(c);
            }
        }

        return true;
    }
}

import java.util.List;
import java.util.ArrayList;
class Test {
    public static void main(String args[]) {
	//System.out.println("hello world");
	//System.out.println("hello iris :3");
	//int[] arr = new int[] {1, 2, 3, 4, 5, 6, 7, 8, 9};
	
	//System.out.println(bsearch(arr, 1));
	//System.out.println(bsearchr(arr, 3));
	//	System.out.println(addString("123", "124"));
	System.out.println(rotateString("abcde", "cdeab"));
    }

    public static List<Integer> majorityElement(int[] nums) {
        HashMap<Integer, Integer> map = new HashMap<>();

	for (int i : nums) {
	    if (map.containsKey(i)) map.put(i, map.get(i) + 1);
	    else map.put(i, 1);
	}

	List<Integer> ans = new ArrayList<>();
	for (int i : map.keySet()) {
	    if (map.get(i) > nums.length / 3) ans.add(i);
	}
	return ans;
    }

    public static boolean rotateString(String s, String goal) {
        // abcde, cdeab
	// abcab, ababc
        
	if (s.length() != goal.length()) return false;
	StringBuilder sb = new StringBuilder(s);
	StringBuilder gb = new StringBuilder(goal);
	for (int i = 0; i < s.length(); i++) {
	    System.out.println(sb.toString());
	    System.out.println(gb.toString());
	    System.out.println(" ");
	    if (sb.toString().equals(gb.toString())) return true;
	    else {
		char temp = sb.charAt(0);
		sb.deleteCharAt(0);
		sb.append(temp);
	    }
	}
	return false;
    }
    
}

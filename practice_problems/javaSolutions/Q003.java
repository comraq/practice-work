import java.util.*;

public class Q003 {
  public int lengthOfLongestSubstring(String s) {
    int max = 0;
    int start = 0;
    Map<Character, Integer> table = new HashMap<>();

    for (int i = 0; i < s.length(); ++i) {
      char c = s.charAt(i);
      if (table.containsKey(c) && table.get(c) >= start) {
        max = (i - start > max)? i - start : max;
        start = table.get(c) + 1;
      }
      table.put(c, i);
    }
    start = s.length() - start;
    return (start > max)? start : max;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q003 sol = new Q003();
    for (String s : ex)
      System.out.println(sol.lengthOfLongestSubstring(s));

  }
}

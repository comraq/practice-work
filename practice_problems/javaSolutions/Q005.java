import java.util.*;

public class Q005 {
  public String longestPalindrome(String s) {
    if (s == null || s.length() < 2)
      return s;

    String result = s.substring(0, 1);
    String temp;
    for (int i = 1; i < s.length(); ++i) {
      if (s.charAt(i - 1) == s.charAt(i)) {
        temp = isPali(s, i - 2, i + 1);
        result = (temp.length() > result.length())? temp : result;
      }

      if (i != s.length() - 1 && s.charAt(i - 1) == s.charAt(i + 1)) {
        temp = isPali(s, i - 2, i + 2);
        result = (temp.length() > result.length())? temp : result;
      }
    }
    return result;
  }

  private String isPali(String s, int left, int right) {
    while (left >= 0 && right < s.length()) {
      if (s.charAt(left) != s.charAt(right))
        break;

      --left;
      ++right;
    }
    return s.substring(left + 1, right);
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q005 sol = new Q005();
    for (String s : ex)
      System.out.println(sol.lengthOfLongestSubstring(s));

  }
}

import java.util.*;

public class Q242 {
  public boolean isAnagram(String s, String t) {
    Map<Character, Integer> table = new HashMap<>();
    if (s == null || t == null || t.length() != s.length()) 
      return false;

    for (char c: s.toCharArray()) {
      if (!table.containsKey(c))
        table.put(c, 1);
      else {
        int count = table.get(c);
        table.put(c, count + 1);
      }
    }
    for (char c: t.toCharArray()) {
      if (!table.containsKey(c))
        return false;
      else {
        int count = table.get(c);
        if (count < 1)
          return false;
        table.put(c, count - 1);
      }
    }
    return true;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q242 sol = new Q242();
    System.out.println(sol.isAnagram("asdfljwe", "a"));
    System.out.println(sol.isAnagram("asdfljwe", ""));
    System.out.println(sol.isAnagram("", ""));
    System.out.println(sol.isAnagram("ajwe", "asdfljwe"));
    System.out.println(sol.isAnagram("drow", "word"));
    System.out.println(sol.isAnagram("wordbank", "wordl"));
  }
}

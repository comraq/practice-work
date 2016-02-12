import java.util.*;

public class Q205 {
  public boolean isIsomorphic(String s, String t) {
    Map<Character, Character> table = new HashMap<>();
    for (int i = 0; i < s.length(); ++i) {
      char sChar = s.charAt(i);
      char tChar = t.charAt(i);
      if (table.containsKey(sChar)) {
        if (table.get(sChar) != tChar)
          return false;
      } else if (table.containsValue(tChar))
        return false;
      else
        table.put(sChar, tChar);
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

    int[] a = {-19,-3,-1000,-432,-200,-199,-100};
    Q205 sol = new Q205();
    System.out.println(sol.findMin(a));
  }
}

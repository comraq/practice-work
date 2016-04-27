import java.util.*;

public class Q316 {
  public String removeDuplicateLetters(String s) {
    Map<Character, Integer> table = new HashMap<>();
    for (char c: s.toCharArray()) {
      if (!table.containsKey(c))
        table.put(c, 1);
      else
        table.put(c, table.get(c) + 1);
    } 

    Stack<Character> stack = new Stack<>();
    HashSet<Character> boolSet = new HashSet<>();
    for (char c: s.toCharArray()) {
      if (boolSet.contains(c) && table.get(c) > 1) {
        table.put(c, table.get(c) - 1);
        continue;
      }

      while (!stack.isEmpty()) {
        char top = stack.peek();
        if ((int)c < (int)top && table.get(top) > 1) {
          stack.pop();
          table.put(top, table.get(top) - 1);
          boolSet.remove(top);
        } else
          break;
      }
      stack.push(c);
      boolSet.add(c);
    }

    String result = "";
    while (!stack.isEmpty())
      result = stack.pop() + result;

    return result;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    int[] a = {198,-3,38,0,1,-1,3,2,0};
    Q316 sol = new Q316();
  }
}

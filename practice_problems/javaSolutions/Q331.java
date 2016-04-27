import java.util.*;

import myUtils.*;

public class Q331 {
  public boolean isValidSerialization(String preorder) {
    if (preorder == null || preorder.equals(""))
      return false;

    String[] input = preorder.split(",");
    int remain = 1;
    for (int i = 0; i < input.length; ++i) {
      if (input[i].matches("\\d"))
        ++remain;
      else if (input[i].equals("#")) {
        if (--remain == 0 && i != input.length - 1)
          return false;
      }
    }
    if (remain != 0)
      return false;

    return true;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    int[] a = {198,-3,38,0,1,-1,3,2,0};
    Q331 sol = new Q331();
    System.out.println("1293".matches("\\d+"));
  }
}

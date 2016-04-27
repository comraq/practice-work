import java.util.*;

public class Q007 {
  public int reverse(int x) {
    int result = 0;
    int sign = 1;
    if (x < 0) {
      sign = -1;
      x *= sign;
    }
    while (x > 0) {
      if ((result > Integer.MAX_VALUE / 10)
          || (result == Integer.MAX_VALUE / 10 && x % 10 > Integer.MAX_VALUE % 10))
        return 0;
      if ((result < Integer.MIN_VALUE / 10)
          || (result == Integer.MIN_VALUE / 10 && x % 10 < Integer.MIN_VALUE % 10))
        return 0;

      result = 10 * result + x % 10;
      x /= 10;
    }
    return result * sign;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    int[] testCase = {0, 123, -123, 1000000003, 1000, -2143847412};
    Q007 sol = new Q007();
    for (int i = 0; i < testCase.length; ++i)
      System.out.println(sol.reverse(testCase[i]));
  }
}

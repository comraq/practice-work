import java.util.*;

public class Q070 {
  public int climbStairs(int n) {
    if (n < 2)
      return 1;
 
    int[] cache = new int[n];
    return climbRecurse(cache, n);
  }

  private int climbRecurse(int[] cache, int step) {
    int index = step - 1;
    if (cache[index] != 0)
      return cache[index];

    if (step < 3) {
      cache[index] = step;
      return step;
    }
    int result = climbRecurse(cache, step - 1) + climbRecurse(cache, step - 2);
    cache[index] = result;
    return result;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q070 sol = new Q070();
    int[] test = {0,2,5,4,10,44};
    for (int i = 0; i < test.length; ++i)
      System.out.println(sol.climbStairs(test[i]));
  }
}

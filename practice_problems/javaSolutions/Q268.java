import java.util.*;

public class Q268 {
  public int missingNumber(int[] nums) {
    if (nums == null || nums.length < 1)
      return 0;

    int n = nums.length;
    int result = n;
    for (int i = 0; i < nums.length; ++i) {
      result ^= i;
      result ^= nums[i];
    }
    return result;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q268 sol = new Q268();
  }
}

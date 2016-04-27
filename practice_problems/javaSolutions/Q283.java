import java.util.*;

import myUtils.*;

public class Q283 {
  public void moveZeroes(int[] nums) {
    if (nums == null || nums.length < 1)
      return;
 
    int nzIndex = 0;
    for (int i = 0; i < nums.length; ++i) {
      if (nums[i] != 0) {
        if (nzIndex != i)
          nums[nzIndex] = nums[i];

        ++nzIndex;
      }
    }
    while (nzIndex < nums.length)
      nums[nzIndex++] = 0;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    int[] a = {198,-3,38,0,1,-1,3,2,0};
    Q283 sol = new Q283();
  }
}

import java.util.*;

import myUtils.*;

public class Q027 {
  public int removeElement(int[] nums, int val) {
    if (nums == null || nums.length < 1)
      return 0;

    int last = nums.length - 1;
    for (int i = 0; i < last; ++i) {
      while (last >= 0 && nums[last] == val) {
        if (i >= --last)
        break;
      }

      if (nums[i] == val)
        nums[i] = nums[last--];
    }
    while (last >= 0 && nums[last] == val)
      --last;

    return last + 1;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    int[] a = {198,-3,38,0,1,-1,3,2,0};

    int n = 7;
    Q027 sol = new Q027();
  }
}

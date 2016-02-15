import java.util.*;

import myUtils.*;

public class Q031 {
  public void nextPermutation(int[] nums) { 
    if (nums == null || nums.length < 2)
      return;

    int start = 0;
    for (int i = nums.length - 1; i > 0; --i) {
      if (nums[i] > nums[i - 1]) {
        int temp = nums[i - 1];
        for (int j = i; j < nums.length; ++j) {
          if (j == nums.length - 1 || nums[j + 1] <= temp) {
            nums[i - 1] = nums[j];
            nums[j] = temp;
            break;
          }
        }
        start = i;
        break;
      }
    }

    int end = nums.length - 1;
    while (start < end) {
      int temp = nums[start];
      nums[start] = nums[end];
      nums[end] = temp;
      --end;
      ++start;
    }
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q031 sol = new Q031();
    /*String[] s = {"asdf", ".,wr23", "192inf-", "/wep23rn"};
    System.out.println(Arrays.toString(s));
    Arrays.sort(s);
    s[2] = null;
    System.out.println(Arrays.toString(s));*/
  }
}

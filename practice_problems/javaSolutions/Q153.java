import java.util.*;

public class Q153 {
  public int findMin(int[] nums) {
    return minRecurse(nums, 0, nums.length - 1);
  }

  private int minRecurse(int[] nums, int low, int high) {
    if (nums.length < 1)
      return -1;
    else if (low >= high)
      return nums[low];

    int mid = (high + low) / 2;
    if (nums[mid] > nums[high])
      return minRecurse(nums, mid + 1, high);
    else
      return minRecurse(nums, low, mid);

  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    int[] a = {-19,-3,-1000,-432,-200,-199,-100};
    Q153 sol = new Q153();
    System.out.println(sol.findMin(a));
  }
}

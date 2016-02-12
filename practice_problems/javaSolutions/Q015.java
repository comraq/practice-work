import java.util.*;

public class Q015 {
  public List<List<Integer>> threeSum(int[] nums) {
    List<List<Integer>> result = new ArrayList<>();
    if (nums == null || nums.length < 3)
      return result;

    Arrays.sort(nums);
    for (int i = 0; i < nums.length - 2; ++i) {
      int first = nums[i];
      if (first > 0)
        break;
      if (i > 0 && first == nums[i - 1])
        continue;

      int low = i + 1;
      int high = nums.length - 1;
      while (low < high) {
        if (nums[low] + nums[high] + first == 0) {
          result.add(Arrays.asList(first, nums[low],
nums[high]));

          while (low++ < high && nums[low] == nums[low - 1]);
          while (low < high-- && nums[high] == nums[high + 1]);

        } else if (nums[low] + nums[high] + first > 0)
          --high;

        else
          ++low;
      }
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

    int[] a = {198,-3,38,0,1,-1,3,2,0};
    Q015 sol = new Q015();
    System.out.println(sol.threeSum(a));
  }
}

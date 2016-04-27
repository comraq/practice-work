import java.util.*;

public class Q053 {
  public int maxSubArray(int[] nums) {
    if (nums == null || nums.length < 1)
      return Integer.MIN_VALUE;

    if (nums.length == 1)
      return nums[0];
 
    int result = Integer.MIN_VALUE;
    int curr = nums[0];
    for (int i = 1; i < nums.length; ++i) {
      if (nums[i] < 0) {
        result = (curr > result)? curr : result;
        if (curr + nums[i] < 0)
          curr = nums[i];
        else
          curr += nums[i];

      } else {
        if (curr < 0)
          curr = nums[i];
        else
          curr += nums[i];
      }
    }
    return (curr > result)? curr : result;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q053 sol = new Q053();
    int[][] test = {{-2,1,-3,4,-1,2,1,-5,4},
                    {-2,-1},
                    {8,-19,5,-4,20},
                    {-2,1},
                    {-2,-3,-1}};
    
    for (int i = 0; i < test.length; ++i)
      System.out.println(sol.maxSubArray(test[i]));
  }
}

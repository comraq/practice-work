import java.util.*;

public class Q239 {
  public int[] maxSlidingWindow(int[] nums, int k) {
    if (nums.length < 1)
      return nums;

    int[] result = new int[nums.length - k + 1];
    int currMax = Integer.MIN_VALUE;
    Deque<Integer> queue = new ArrayDeque<>();
    for (int i = 0; i < k; ++i) {
      currMax = (nums[i] > currMax)? nums[i] : currMax;
      queue.add(nums[i]);
    }
    result[0] = currMax;

    for (int r = 1; r < result.length; ++r) {
      System.out.println("max: " + currMax);
      System.out.println(queue);

      int index = r + k - 1;
        int last = queue.removeFirst();
        queue.add(nums[index]);
        if (nums[index] > currMax)
          currMax = nums[index];
        else if (currMax == last) {
          currMax = Integer.MIN_VALUE;
          for (int i = r; i <= index; ++i)
            currMax = (nums[i] > currMax)? nums[i] : currMax;
        }
      result[r] = currMax;
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

    int[] a = {1,3,-1,-3,5,3,6,7};

    int n = 7;
    int[][] testCase = {{1,0},{1,2},{1,3},{2,4},{0,5},{4,6}};
    Q239 sol = new Q239();
    sol.maxSlidingWindow(a, 3);
  }
}

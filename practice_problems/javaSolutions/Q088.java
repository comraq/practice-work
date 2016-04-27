import java.util.*;

public class Q088 {
  public void merge(int[] nums1, int m, int[] nums2, int n) {
    if (nums1 == null || nums2 == null
        || nums1.length != n + m || n == 0)
      return;

    if (m == 0) {
      for (int i = n - 1; i >= 0; --i)
        nums1[i] = nums2[i];

      return;
    }
 
    int index1 = m - 1, index2 = n - 1;
    for (int i = nums1.length - 1; i >= 0; --i) {
      if (index2 >= 0 && (index1 < 0 || nums2[index2] >= nums1[index1]))
        nums1[i] = nums2[index2--];
      else
        nums1[i] = nums1[index1--];
    }
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q088 sol = new Q088();
    int[] test = {0,2,5,4,10,44};
    for (int i = 0; i < test.length; ++i)
      System.out.println(sol.climbStairs(test[i]));
  }
}

import java.util.*;

public class Q189 {
  public void rotate(int[] nums, int k) {
    if (nums == null || nums.length < 1)
      return;
      
    k = k % nums.length;
    if (k < 1)
      return;

    int factor = 1;
    if (k > 3) {
      if (k % 3 == 0) {
        factor = k / 3;
        k = 3;
      } else if (k % 2 == 0) {
        factor = k / 2;
        k = 2;
      }
    }

    for (int f = 0; f < factor; ++f) {
      int iter = (nums.length % k == 0)? k : 1;
      for (int i = 0; i < iter; ++i) {
        int j = i;
        int curr = nums[j];
        int next;
        do {
          next = nums[(j + k) % nums.length];
          nums[(j + k) % nums.length] = curr;
          j = (j + k) % nums.length;
          curr = next;
        } while (j != i);
      }
    }
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    int[] a = {198,-3,38,0,1,-1,3,2,0};
    int[] b = {};
    int[] c = {-711};
    int[] d = {1,2,3,4,5,6};
    int[] e =
{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54};
   
    Q189 sol = new Q189();
    sol.rotate(a, 3);
    sol.rotate(a, 0);
    sol.rotate(b, 2);
    sol.rotate(c, 2);
    sol.rotate(d, 4);
    sol.rotate(e, 45);
    System.out.println(Arrays.toString(d));
    System.out.println(Arrays.toString(e));
  }
}

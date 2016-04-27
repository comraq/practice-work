import java.util.*;

public class Q279 {
  public int numSquares(int n) {
    int[] cache = new int[n + 1];
    cache[0] = 0;
    for (int num = 1; num <= n; ++num) {
      int root = (int) Math.sqrt(num);
      if (root * root == num) {
        cache[num] = 1;
        continue;
      }

      for (int i = 1; i <= root; ++i) {
        int offset = num - i * i;
        if (cache[num] == 0 || cache[num] > cache[offset] + 1)
          cache[num] = cache[offset] + 1;
      }     
    }
    return cache[n];
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
    int[][] testCase = {{1,0},{1,2},{1,3},{2,4},{0,5},{4,6}};
    Q279 sol = new Q279();
    System.out.println(sol.numSquares(13));
    System.out.println(sol.numSquares(41));
    System.out.println(sol.numSquares(12));
  }
}

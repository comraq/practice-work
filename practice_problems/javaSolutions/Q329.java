import java.util.*;

import myUtils.*;

public class Q329 {
  public int longestIncreasingPath(int[][] matrix) {
    if (matrix == null || matrix.length < 1 || matrix[0].length < 1)
      return 0;
 
    int[] cache = new int[matrix.length * matrix[0].length];
    int longest = 1;
    for (int r = 0; r < matrix.length; ++r) {
      for (int c = 0; c < matrix[0].length; ++c) {
        int temp = longPathRecurse(cache, matrix, r, c);
        longest = (temp > longest)? temp : longest;
        if (longest == cache.length)
          return longest;
      }
    }
    return longest;
  }

  private int longPathRecurse(int[] cache, int[][] matrix, int row, int col) {
    int index = row * matrix[0].length + col;
    if (cache[index] != 0)
      return cache[index];

    int result = 1;
    int val = matrix[row][col];
    if (row > 0 && val > matrix[row - 1][col]) {
      int temp = 1 + longPathRecurse(cache, matrix, row - 1, col);
      result = (temp > result)? temp : result;
    }

    if (row < matrix.length - 1 && val > matrix[row + 1][col]) {
      int temp = 1 + longPathRecurse(cache, matrix, row + 1, col);
      result = (temp > result)? temp : result;
    }

    if (col > 0 && val > matrix[row][col - 1]) {
      int temp = 1 + longPathRecurse(cache, matrix, row, col - 1);
      result = (temp > result)? temp : result;
    }

    if (col < matrix[0].length - 1 && val > matrix[row][col + 1]) {
      int temp = 1 + longPathRecurse(cache, matrix, row, col + 1);
      result = (temp > result)? temp : result;
    }

    cache[index] = result;
    return result;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q329 sol = new Q329();
    /*String[] s = {"asdf", ".,wr23", "192inf-", "/wep23rn"};
    System.out.println(Arrays.toString(s));
    Arrays.sort(s);
    s[2] = null;
    System.out.println(Arrays.toString(s));*/
    int[][] a = {{9,9,4},
                 {6,6,8},
                 {2,1,1}};
    int[][] b = {{3,4,5},
                 {3,2,6},
                 {2,2,1}};
    System.out.println(sol.longestIncreasingPath(a));
    System.out.println(sol.longestIncreasingPath(b));
  }
}

import java.util.*;

import myUtils.*;

public class Q073 {
  public void setZeroes(int[][] matrix) {
    if (matrix == null || matrix.length < 1 || matrix[0].length < 1)
      return;

    boolean left = false, top = false;
    if (matrix[0][0] == 0) {
      matrix[0][0] = 0;
      left = true;
      top = true;
    }
    for (int row = 0; row < matrix.length; ++row) {
      for (int col = 0; col < matrix[0].length; ++col) {
        if (row == 0 && matrix[0][col] == 0) top = true;
        if (col == 0 && matrix[row][0] == 0) left = true;

        if (matrix[row][col] == 0) {
          matrix[0][col] = 0;
          matrix[row][0] = 0;
        }
      }
    }
    for (int r = 1; r < matrix.length; ++r) {
      if (matrix[r][0] == 0) {
        for (int c = 1; c < matrix[0].length; ++c)
          matrix[r][c] = 0;
      }
      if (left) matrix[r][0] = 0;
    }
    for (int c = 1; c < matrix[0].length; ++c) {
      if (matrix[0][c] == 0) {
        for (int r = 1; r < matrix.length; ++r)
          matrix[r][c] = 0;
      }
      if (top) matrix[0][c] = 0;
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
    Q073 sol = new Q073();
  }
}

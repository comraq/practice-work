import java.util.*;

public class Q054 {
  public List<Integer> spiralOrder(int[][] matrix) {
    if (matrix == null || matrix.length < 1 || matrix[0].length < 1)
      return new ArrayList<Integer>();

    int minSize = (matrix.length < matrix[0].length)? matrix.length : matrix[0].length;
    int layers = (minSize > 2)? ((minSize + 2 - 1) / 2) : 1;
    List<Integer> result = new ArrayList<>();

    for (int layer = 0; layer < layers; ++layer)
      spiralRecurse(matrix, result, layer, layer,
                    matrix.length - layer, matrix[0].length - layer);

    return result;
  }

  private void spiralRecurse(int[][] matrix, List<Integer> result, int top,
                             int left, int bottom, int right) {
    if (top >= bottom || left >= right)
      return;

    if (top + 1 == bottom) {
      for (int i = left; i < right; ++i)
        result.add(matrix[top][i]);

      return;
    } else if (left + 1 == right) {
      for (int i = top; i < bottom; ++i)
        result.add(matrix[i][left]);

      return;
    }

    // Top Row
    for (int i = left; i < right - 1; ++i)
      result.add(matrix[top][i]);

    // Right Col
    for (int i = top; i < bottom - 1; ++i)
      result.add(matrix[i][right - 1]);

    // Bottom Row
    for (int i = right - 1; i > left; --i)
      result.add(matrix[bottom - 1][i]);

    // Left Col
    for (int i = bottom - 1; i > top; --i)
      result.add(matrix[i][left]);
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    int[][][] a = {{{ 1, 2, 3 },
                    { 4, 5, 6 },
                    { 7, 8, 9 }},

                   {{ 1, 2, 3, 4 },
                    { 5, 6, 7, 8 },
                    { 9,10,11,12 }},

                   {{ 1, 2, 3, 4 },
                    { 5, 6, 7, 8 },
                    { 9,10,11,12 },
                    {13,14,15,16 }},
 
                   {{ 1, 2, 3, 4, 5 },
                    { 6, 7, 8, 9,10 },
                    {11,12,13,14,15 },
                    {16,17,18,19,20 },
                    {21,22,23,24,25 }},
 
                   {{ 1, 2, 3, 4 },
                    { 6, 7, 8, 9 },
                    {11,12,13,14 },
                    {16,17,18,19 },
                    {21,22,23,24 }},

                   {{ 1, 2, 3 },
                    { 6, 7, 8 },
                    {11,12,13 },
                    {16,17,18 },
                    {21,22,23 }},

                   {{ 1, 2 },
                    { 3, 4 },
                    { 5, 6 }},
 
                   {{ 1, 2, 3}},

                   {{ 1, 2, 3 },
                    { 4, 5, 6 },
                    { 7, 8, 9 },
                    {10,11,12 }}};

    Q054 sol = new Q054();
    for (int i = 0; i < a.length; ++i)
      System.out.println(sol.spiralOrder(a[i]));
  }
}

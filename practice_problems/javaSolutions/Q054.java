import java.util.*;

public class Q054 {
  public List<Integer> spiralOrder(int[][] matrix) {
    if (matrix == null || matrix.length < 1 || matrix[0].length < 1)
      return null;

    int minSize = (matrix.length < matrix[0].length)? matrix.length :
matrix[0].length;
    int layers = (minSize > 2)? ((minSize + 2 - 1) / 2) : 1;
    List<Integer> result = new ArrayList<>();
        
    for (int layer = 0; layer < layers; ++layer) {
      // Top Row
      for (int i = layer; i < matrix[0].length - layer - 1; ++i)
        result.add(matrix[layer][i]);

      // Right Col
      for (int i = layer; i < matrix.length - layer - 1; ++i)
        result.add(matrix[i][matrix[0].length - 1 - layer]);

      // Bottom Row
      for (int i = matrix[0].length - 1 - layer; i > layer; --i)
        result.add(matrix[matrix.length - 1 - layer][i]);

      // Left Col
      for (int i = matrix.length - 1 - layer; i > layer; --i)
        result.add(matrix[i][layer]);
    }
    if (matrix.length == matrix[0].length
        && (matrix.length * matrix.length) % 2 == 1)
      result.add(matrix[layers - 1][layers - 1]);

    return result;
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

                   {{ 1, 2 },
                    { 3, 4 },
                    { 5, 6 }},
 
                   {{ 1, 2, 3 },
                    { 4, 5, 6 },
                    { 7, 8, 9 },
                    {10,11,12 }}};

    Q054 sol = new Q054();
    for (int i = 0; i < a.length; ++i)
      System.out.println(sol.spiralOrder(a[i]));
  }
}

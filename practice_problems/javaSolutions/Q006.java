import java.util.*;

public class Q006 {
  public String convert(String s, int numRows) {
    if (s == null || s.length() <= numRows)
      return s;

    String[] results = new String[numRows];
    int change = 1;
    int row = 0;
    for (Character c: s.toCharArray()) {
      if (results[row] == null)
        results[row] = "" + c;
      else
        results[row] += c;

      if (row == 0)
        change = 1;
      else if (row == numRows - 1)
        change = -1;

      row += change;
    }

    String result = "";
    for (int i = 0; i < results.length; ++i)
      result += results[i];

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

    int n = 7;
    int[][] testCase = {{1,0},{1,2},{1,3},{2,4},{0,5},{4,6}};
    Q006 sol = new Q006();
    String[] something = new String[2];
    System.out.println(Arrays.toString(something));
  }
}

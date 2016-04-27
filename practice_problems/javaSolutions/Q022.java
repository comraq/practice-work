import java.util.*;

public class Q022 {
  public List<String> generateParenthesis(int n) {
    if (n < 1)
      return new ArrayList<String>();
 
    return genRecurse(new ArrayList<String>(), new char[2 * n], n, n);
  }

  private List<String> genRecurse(List<String> result, char[] buffer,
                                  int lRemain, int rRemain) {
    if (lRemain == 0 && rRemain == 0)
      result.add(String.copyValueOf(buffer));
    else {
      int index = buffer.length - lRemain - rRemain;
      if (lRemain > 0) {
        buffer[index] = '(';
        genRecurse(result, buffer, lRemain - 1, rRemain);
      }

      if (rRemain > lRemain) {
        buffer[index] = ')';
        genRecurse(result, buffer, lRemain, rRemain - 1);
      }
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

    Q022 sol = new Q022();
    int[][] test = {{-2,1,-3,4,-1,2,1,-5,4},
                    {-2,-1},
                    {8,-19,5,-4,20},
                    {-2,1},
                    {-2,-3,-1}};
    
    System.out.println(sol.generateParenthesis(3));
    System.out.println(sol.generateParenthesis(5));
    System.out.println(sol.generateParenthesis(1));
  }
}

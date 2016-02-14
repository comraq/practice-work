import java.util.*;

public class Q008 {
  public int myAtoi(String str) { 
    int result = 0;
    if (str == null || str.equals(""))
      return result;

    str = str.trim();
    int start = 0;
    int sign = 1;
    if (str.charAt(start) == '+' || str.charAt(start) == '-') {
      if (str.charAt(start++) == '-')
        sign = -1;
    }
    int end = start;
    while (end < str.length() && str.substring(end, end + 1).matches("\\d"))
      ++end;

    if (start == end)
      return result;

    try {
      result = Integer.parseInt(str.substring(start, end)) * sign;
    } catch (Exception e) {
      result = (sign > 0)? Integer.MAX_VALUE : Integer.MIN_VALUE;
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

    Q008 sol = new Q008();
    System.out.println(Integer.MAX_VALUE);
  }
}

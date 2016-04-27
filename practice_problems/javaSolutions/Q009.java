import java.util.*;

public class Q009 {
  public boolean isPalindrome(int x) {
    if (x < 0)
      return false;
        
    long mag = 10;
    while (x / mag > 0)
      mag *= 10;

    mag /= 10;
    int high, low;
    while (mag > 0) {
      low = x % 10;
      high = (int) (x / mag);

      if (low != high)
        return false;

      x = (int) ((x % mag) / 10);
      mag /= 100;
    }
    return true;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    int[] a = {198,-3,38,0,1,-1,3,2,0};
    Q009 sol = new Q009();
    System.out.println(sol.isPalindrome(128474821));
    System.out.println(sol.isPalindrome(12844821));
    System.out.println(sol.isPalindrome(1));
    System.out.println(sol.isPalindrome(0));
    System.out.println(sol.isPalindrome(100000));
    System.out.println(sol.isPalindrome(99999));
    System.out.println(sol.isPalindrome(1000000001));
  }
}

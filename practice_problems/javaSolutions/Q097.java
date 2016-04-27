import java.util.*;

public class Q097 {
  public boolean isInterleave(String s1, String s2, String s3) {
    if (s1.length() + s2.length() != s3.length())
      return false;
 
    Set<Integer> cache = new HashSet<>();
    return dfsRecurse(s1, s2, s3, 0, 0, cache);
  }

  private boolean dfsRecurse(String s1, String s2, String s3, int p1, int p2, Set<Integer> cache) {
    int p3 = p1 + p2;
    if (p3 == s3.length())
      return true;

    if (cache.contains(p1 * (s2.length() + 1) + p2))
      return false;

    boolean result = false;
    if (p1 < s1.length() && s1.charAt(p1) == s3.charAt(p3))
      result = result || dfsRecurse(s1, s2, s3, p1 + 1, p2, cache);

    if (p2 < s2.length() && s2.charAt(p2) == s3.charAt(p3))
      result = result || dfsRecurse(s1, s2, s3, p1, p2 + 1, cache);

    if (!result)
      cache.add(p1 * (s2.length() + 1) + p2);
    
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

    String s1 = "aabcc";
    String s2 = "dbbca";
    String s3 = "aadbbcbcac";
    String s4 = "aadbbbaccc";

    String s5 = "aa";
    String s6 = "ab";
    String s7 = "aaba";
    Q097 sol = new Q097();
    System.out.println(sol.isInterleave(s1, s2, s3));
    System.out.println(sol.isInterleave(s1, s2, s4));
    System.out.println(sol.isInterleave(s5, s6, s7));
  }
}

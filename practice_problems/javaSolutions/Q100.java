import java.util.*;

import myUtils.*;

public class Q100 {
  public boolean isSameTree(TreeNode p, TreeNode q) {
    if (p == null && q == null)
      return true;
    else if (p == null || q == null) 
      return false;

    if (p.val != q.val)
      return false;

    return isSameTree(p.left, q.left) && isSameTree(p.right, q.right);
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    int[] a = {198,-3,38,0,1,-1,3,2,0};
    Q100 sol = new Q100();
  }
}

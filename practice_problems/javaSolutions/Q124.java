import java.util.*;

import myUtils.*;

public class Q124 {
  public int maxPathSum(TreeNode root) {
    if (root == null)
      return Integer.MIN_VALUE;
    else {
      int[] result = {Integer.MIN_VALUE};
      int rootSum = maxRecurse(root, result);
      return (rootSum > result[0])? rootSum : result[0];
    }
  }

  private int maxRecurse(TreeNode node, int[] currMax) {
    int lSum, rSum, cSum;

    if (node.left == null && node.right == null) {
      currMax[0] = (node.val > currMax[0])? node.val : currMax[0];
      return node.val;

    } else if (node.right == null) {
      lSum = maxRecurse(node.left, currMax);
      cSum = node.val + ((lSum > 0)? lSum : 0);
      currMax[0] = (cSum > currMax[0])? cSum : currMax[0];
      return cSum;

    } else if (node.left == null) {
      rSum = maxRecurse(node.right, currMax);
      cSum = node.val + ((rSum > 0)? rSum : 0);
      currMax[0] = (cSum > currMax[0])? cSum : currMax[0];
      return cSum;

    } else {
      lSum = maxRecurse(node.left, currMax);
      rSum = maxRecurse(node.right, currMax);
      cSum = ((lSum > 0)? lSum : 0) + ((rSum > 0)? rSum : 0) + node.val;
      currMax[0] = (cSum > currMax[0])? cSum : currMax[0];
      int maxPath = (lSum > rSum)? lSum : rSum;
      return node.val + ((maxPath > 0)? maxPath : 0);
    }
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    int[] a = {-19,-3,-1000,-432,-200,-199,-100};
    Q124 sol = new Q124();
  }
}

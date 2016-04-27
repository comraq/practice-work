import java.util.*;

import myUtils.*;

public class Q113 {
  public List<List<Integer>> pathSum(TreeNode root, int sum) {
    List<List<Integer>> result = new ArrayList<List<Integer>>();
    if (root == null)
      return result;

    pathSumRecurse(result, new ArrayList<Integer>(), root, sum);
    return result;
  }

  private void pathSumRecurse(List<List<Integer>> result,
                              List<Integer> path, TreeNode node, int val) {
    if (node.left == null && node.right == null) {
      int sum = node.val;
      for (Integer nodeVal: path)
        sum += nodeVal;

      if (sum == val) {
        List<Integer> foundPath = (ArrayList<Integer>) ((ArrayList<Integer>) path).clone();
        foundPath.add(node.val);
        result.add(foundPath);
      }
      return;
    }

    path.add(node.val);
    if (node.left != null)
      pathSumRecurse(result, path, node.left, val);

    if (node.right != null)
      pathSumRecurse(result, path, node.right, val);

    path.remove(path.size() - 1);
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q113 sol = new Q113();
    int[][] test = {{-2,1,-3,4,-1,2,1,-5,4},
                    {-2,-1},
                    {8,-19,5,-4,20},
                    {-2,1},
                    {-2,-3,-1}};
  }
}

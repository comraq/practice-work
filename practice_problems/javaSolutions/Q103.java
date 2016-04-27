import java.util.*;

import myUtils.*;

public class Q103 {
  public List<List<Integer>> zigzagLevelOrder(TreeNode root) {
    List<List<Integer>> result = new ArrayList<>();
    if (root == null)
      return result;

    Deque<TreeNode> nodes = new ArrayDeque<>();
    nodes.add(root);
    zigzagRecurse(nodes, result, false);
    return result;
  }

  private void zigzagRecurse(Deque<TreeNode> nodes,
                             List<List<Integer>> result, boolean reverse) {
    if (nodes == null || nodes.isEmpty())
      return;

    List<Integer> rowVals = new ArrayList<>();
    int nodesLength = nodes.size();
    for (int i = 0; i < nodesLength; ++i) {
      TreeNode curr = (reverse)? nodes.removeLast() : nodes.removeFirst();
      rowVals.add(curr.val);

      if (reverse) {
        if (curr.right != null)
          nodes.addFirst(curr.right);
        if (curr.left != null)
          nodes.addFirst(curr.left);

      } else {
        if (curr.left != null)
          nodes.addLast(curr.left);
        if (curr.right != null)
          nodes.addLast(curr.right);
      }
    }
    result.add(rowVals);
    zigzagRecurse(nodes, result, !reverse);
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q103 sol = new Q103();
    List<Integer> test = new ArrayList<>();
    test.add(3);
    test.add(9);
    test.add(20);
    test.add(null);
    test.add(null);
    test.add(15);
    test.add(7);
      
    TreeNode root = TreeNode.deserialize(test);
    System.out.println(sol.zigzagLevelOrder(root));
  }
}

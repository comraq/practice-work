import java.util.*;

import myUtils.*;

public class Q114 {
  public void flatten(TreeNode root) {
    if (root == null)
      return;

    flatRecurse(root);
  }

  private TreeNode flatRecurse(TreeNode root) {
    if (root == null)
      return null;
 
    TreeNode lHalf = flatRecurse(root.left);
    TreeNode rHalf = flatRecurse(root.right);
    if (lHalf == null)
      root.right = rHalf;
    else {
      root.right = lHalf;
      while (lHalf.right != null)
        lHalf = lHalf.right;
    
      lHalf.right = rHalf;
    }
    root.left = null;
    return root;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    List<Integer> nodeList = new ArrayList<>(Arrays.asList(
      4,5,null,4,6,null,null,7,null,8,null,null,9,10));
    TreeNode tree = TreeNode.deserialize(nodeList);
    TreeNode.printTree(tree);
    System.out.println(TreeNode.serialize(tree));

    Q114 sol = new Q114();
    TreeNode head = sol.toList(tree);
    List<Integer> result = new ArrayList<>();
    while (head != null) {
      result.add(head.val);
      head = head.right;
    }
    System.out.println(result);
  }
}

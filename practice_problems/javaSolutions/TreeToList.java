import java.util.*;

import myUtils.*;

public class TreeToList {
  public TreeNode toList(TreeNode root) {
    if (root == null)
      return root;

    root = inOrderJoin(root);
    while(root.left != null)
      root = root.left;

    return root;
  }

  private TreeNode inOrderJoin(TreeNode root) {
    if (root == null)
      return null;

    if (root.left == null && root.right == null) {
      return root;
    } else if (root.right == null) {
      TreeNode left = inOrderJoin(root.left);
      left.right = root;
      root.left = left;
      return root;
    } else if (root.left == null) {
      TreeNode right = inOrderJoin(root.right);
      root.right = right;
      right.left = root;
      return right;
    } else {
      TreeNode left = inOrderJoin(root.left);
      left.right = root;
      root.left = left;
      TreeNode right = inOrderJoin(root.right);
      root.right = right;
      right.left = root;
      return right;
    }
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

    TreeToList sol = new TreeToList();
    TreeNode head = sol.toList(tree);
    List<Integer> result = new ArrayList<>();
    while (head != null) {
      result.add(head.val);
      head = head.right;
    }
    System.out.println(result);
  }
}

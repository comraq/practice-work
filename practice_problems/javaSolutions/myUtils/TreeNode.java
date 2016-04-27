package myUtils;

import java.util.*;

public class TreeNode {
  public int val;
  public TreeNode left;
  public TreeNode right;

  public TreeNode(int x) {
    val = x;
  }

  public static List<Integer> serialize (TreeNode root) {
    List<Integer> result = new ArrayList<>();
    if (root == null)
      return result;

    List<TreeNode> queue = new ArrayList<>();
    queue.add(root);
    while (!queue.isEmpty()) {
      TreeNode node = queue.remove(0);
      if (node == null)
        result.add(null);
      else {
        result.add(node.val);

        if (node.left != null)
          queue.add(node.left);
        else
          queue.add(null);

        if (node.right != null)
          queue.add(node.right);
        else
          queue.add(null);
      }
    }
    int i = result.size() - 1;
    while (i > 0 && result.get(i) == null) {
      result.remove(i);
      --i;
    }
    return result;
  }

  public static TreeNode deserialize (List<Integer> nodeList) {
    if (nodeList == null || nodeList.size() < 1)
      return null;

    TreeNode root = new TreeNode(nodeList.remove(0));
    TreeNode node;
    List<TreeNode> queue = new ArrayList<>();
    queue.add(root);
    while(!nodeList.isEmpty()) {
      node = queue.remove(0);

      Integer left = nodeList.remove(0);
      if (left != null) {
        node.left = new TreeNode(left);
        queue.add(node.left);
      }
      
      if (!nodeList.isEmpty()) {
        Integer right = nodeList.remove(0);
        if (right != null) {
          node.right = new TreeNode(right);
          queue.add(node.right);
        }
      }
    }
    return root;
  }

  public static void printTree (TreeNode root) {
    int maxWidth = findMaxWidth(root);
    if (root == null)
      return;

    String spaceWidth = "";
    for (int i = 0; i < maxWidth; ++i)
      spaceWidth += " ";

    printRecurse(root, "", spaceWidth);
  }

  private static void printRecurse(TreeNode node,
                                   String spaces, String width) {
    if (node.right != null)
      printRecurse(node.right, spaces + width  + "  ", width);

    System.out.println(spaces + node.val);

    if (node.left != null)
      printRecurse(node.left, spaces + width  + "  ", width);
  }

  private static int findMaxWidth (TreeNode node) {
    if (node == null)
      return 0;

    int lWidth = 0, rWidth = 0;
    int width = Integer.toString(node.val).length();
    if (node.left == null && node.right == null)
      return width;

    if (node.left != null) {
      lWidth = findMaxWidth(node.left);
      width = (lWidth > width)? lWidth : width;
    }

    if (node.right != null) {
      rWidth = findMaxWidth(node.right);
      width = (rWidth > width)? rWidth : width;
    }
    return width;
  }
}

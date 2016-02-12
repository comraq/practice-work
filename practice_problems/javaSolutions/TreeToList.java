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

    int[] a = {198,-3,38,0,1,-1,3,2,0};

    int n = 7;
    int[][] testCase = {{1,0},{1,2},{1,3},{2,4},{0,5},{4,6}};
    TreeToList sol = new TreeToList();
    Heap heap = new Heap(a);
  }

  private class TreeNode {
    int val;
    TreeNode left;
    TreeNode right;
    TreeNode(int x) { val = x; }
  }

  private class ListNode {
    int val;
    ListNode next;
    ListNode(int x) { val = x; }
  }
}

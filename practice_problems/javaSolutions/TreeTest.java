import java.util.*;

import myUtils.*;

public class TreeTest {
  public static void main (String[] args) {
    TreeNode test = new TreeNode(17);
    test.left = new TreeNode(21);
    test.right = new TreeNode(30);
    test.right.right = new TreeNode(9);
    test.left.right = new TreeNode(-4);
    test.right.right.left = new TreeNode(8);
    TreeNode.printTree(test);
    System.out.println(TreeNode.serialize(test));

    TreeNode root = new TreeNode(4);
    root.left = new TreeNode(5);
    root.left.left = new TreeNode(4);
    root.left.right = new TreeNode(6);
    root.left.right.left = new TreeNode(7);
    root.left.right.left.left = new TreeNode(8);
    root.left.right.left.left.right = new TreeNode(9);
    root.left.right.left.left.right.left = new TreeNode(10);
    TreeNode.printTree(root);
    System.out.println(TreeNode.serialize(root));

    List<Integer> nodeList = new ArrayList<>(Arrays.asList(
      4,5,null,4,6,null,null,7,null,8,null,null,9,10));
    TreeNode tree = TreeNode.deserialize(nodeList);
    TreeNode.printTree(tree);
    System.out.println(TreeNode.serialize(tree));

    String s = "1,2,3,4,5,6,7,null,9";
    String[] arr = s.split(",");
    List<Integer> intList = new ArrayList<>();
    System.out.println(Arrays.toString(arr));
    for (int i = 0; i < arr.length; ++i) {
      if (arr[i].equals("null"))
        intList.add(null);
      else
        intList.add(Integer.parseInt(arr[i]));
    }
    System.out.println(intList.toString());
  }
}

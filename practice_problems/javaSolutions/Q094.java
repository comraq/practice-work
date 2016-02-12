import java.util.*;

import myUtils.*;

public class Q094 {
  public List<Integer> inorderTraversal(TreeNode root) {
    List<Integer> result = new ArrayList<>();
    if (root == null)
      return result;

    Stack<TreeNode> stack = new Stack<>();
    TreeNode curr = root;
    while(!stack.isEmpty() || curr != null) {
      if (curr == null)
        curr = stack.pop();
      else if (curr.left != null) {
        stack.push(curr);
        curr = curr.left;
        continue;
      }

      result.add(curr.val);
      curr = curr.right;
    } 
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
    Q094 sol = new Q094();
  }
}

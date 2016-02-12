import java.util.*;

import myUtils.*;

public class Q108 {
  public TreeNode sortedArrayToBST(int[] nums) {
    return toBstRec(nums, 0, nums.length);
  }

  private TreeNode toBstRec(int[] arr, int low, int high) {
    if (arr == null || arr.length == 0 || low >= high)
      return null;

    int mid = (high + low) / 2;
    TreeNode node = new TreeNode(arr[mid]);       
    node.left = toBstRec(arr, low, mid);
    node.right = toBstRec(arr, mid + 1, high);
    return node;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    int[] a = {198,-3,38,0,1,-1,3,2,0};
    Q108 sol = new Q108();
  }
}

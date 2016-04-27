import java.util.*;

import myUtils.*;

public class Q105 {
  public TreeNode buildTree(int[] preorder, int[] inorder) {
    if (preorder == null || preorder.length < 1
        || inorder == null || inorder.length < 1
        || preorder.length != inorder.length)
      return null;

    return null;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q105 sol = new Q105();
    int[][] test = {{-2,1,-3,4,-1,2,1,-5,4},
                    {-2,-1},
                    {8,-19,5,-4,20},
                    {-2,1},
                    {-2,-3,-1}};
    String sample = "alsRLWEKJ23ON2w ,wBe.2(01;l1a";
    System.out.println(sample.replaceAll("[^a-zA-Z]", "").toLowerCase());
  }
}

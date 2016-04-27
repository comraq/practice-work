import java.util.*;

import myUtils.*;

public class Q024 {
  public ListNode swapPairs(ListNode head) {
    if (head == null || head.next == null)
      return head;

    ListNode result = head.next;
    ListNode node = head.next.next;
    head.next.next = head;
    head.next = swapPairs(node);
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
    Q024 sol = new Q024();
  }
}

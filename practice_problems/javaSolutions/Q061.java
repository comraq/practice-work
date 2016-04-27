import java.util.*;

import myUtils.*;

public class Q061 {
  public ListNode rotateRight(ListNode head, int k) {
    if (head == null || k == 0)
      return head;
 
    ListNode fast = head;
    int count = 0;
    while (fast.next != null && count++ < k)
      fast = fast.next;

    if (count < k) {
      int numRotate = k % (count + 1);
      if (numRotate == 0)
        return head;

      fast = head;
      count = 0;
      while (count++ < numRotate)
        fast = fast.next;
    }

    ListNode slow = head;
    while (fast.next != null) {
      fast = fast.next;
      slow = slow.next;
    }

    ListNode newHead = slow.next;
    slow.next = null;
    fast.next = head;
    return newHead;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q061 sol = new Q061();
    int[][] test = {{-2,1,-3,4,-1,2,1,-5,4},
                    {-2,-1},
                    {8,-19,5,-4,20},
                    {-2,1},
                    {-2,-3,-1}};
  }
}

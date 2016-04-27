import java.util.*;

import myUtils.*;

public class Q148 {
  public ListNode sortList(ListNode head) {
    if (head == null || head.next == null)
      return head;

    ListNode slow = head;
    ListNode fast = head.next;
    while (fast != null && fast.next != null) {
      slow = slow.next;
      fast = fast.next.next;
    }
    ListNode half = slow.next;
    slow.next = null;
    return merge(sortList(head), sortList(half));
  }

  private ListNode merge(ListNode fHalf, ListNode sHalf) {
    if (fHalf == null)
      return sHalf;
    if (sHalf == null)
      return fHalf;

    ListNode head = null;
    ListNode curr = null;
    while (fHalf != null && sHalf != null) {
      if (fHalf.val < sHalf.val) {
        if (head == null) {
          head = fHalf;
          curr = head;
        } else {
          curr.next = fHalf;
          curr = curr.next;
        }
        fHalf = fHalf.next;
      } else {
        if (head == null) {
          head = sHalf;
          curr = head;
        } else {
          curr.next = sHalf;
          curr = curr.next;
        }
        sHalf = sHalf.next;
      }
    }
    if (fHalf == null)
      curr.next = sHalf;
    else
      curr.next = fHalf;

    return head;
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
    Q148 sol = new Q148();
  }
}

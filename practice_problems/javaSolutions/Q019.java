import java.util.*;

import myUtils.*;

public class Q019 {
  public ListNode removeNthFromEnd(ListNode head, int n) {
    if (head == null || n < 1)
      return head;
    
    ListNode fast = head;
    for (int i = 1; i < n; ++i) {
      if (fast == null)
        break;
      else
        fast = fast.next;
    }
    if (fast == null)
      return head;
 
    ListNode curr = head, prev = head;
    while (fast.next != null) {
      fast = fast.next;
      curr = curr.next;
      if (prev.next != curr)
        prev = prev.next;
    }
    if (prev == curr)
      return head.next;

    prev.next = curr.next;
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
    Q019 sol = new Q019();
    Heap heap = new Heap(a);
  }
}

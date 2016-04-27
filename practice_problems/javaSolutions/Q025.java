import java.util.*;

import myUtils.*;

public class Q025 {
  /*public ListNode reverseKGroup(ListNode head, int k) {
    if (head == null)
      return null;
    
    int count = 1;
    ListNode first = head;
    ListNode curr = head;
    ListNode prev = head;
    ListNode last;
    while (curr.next != null) {
      curr = curr.next;
      if (++count == k) {
        last = curr;
        curr = curr.next;
        last.next = null;
        prev.next = last;
        last = reverseList(first);
        last.next = curr;
        first = curr;
      }
    }
    return null;
  }*/

  public ListNode reverseList(ListNode node) {
    if (node.next == null)
      return node;

    ListNode prev = reverseList(node.next);
    prev.next = node;
    node.next = null;
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

    int n = 7;
    Q025 sol = new Q025();
    ListNode head = new ListNode(0);
    ListNode node = head;
    for (int i = 1; i < n; ++i) {
      node.next = new ListNode(i);
      node = node.next;
    }
    ListNode first = node;
    ListNode.printList(head);
    sol.reverseList(head);
    ListNode.printList(node);
  }
}

package myUtils;

import java.util.*;

public class ListNode {
  public int val;
  public ListNode next;

  public ListNode(int x) {
    val = x;
  }

  public static void printList(ListNode head) {
    List<Integer> list = new ArrayList<>();
    while (head != null) {
      list.add(head.val);
      head = head.next;
    }
    System.out.println(list);
  }
}

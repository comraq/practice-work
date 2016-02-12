"use strict"
/**
 * Exports a ListNode constructor
 *
 * If not used as a constructor, ListNode takes an array as arguments,
 * converts each value of the array into a linked-list and returns the head
 * of the list
 */
function ListNode(val) {
  if (this instanceof ListNode) {
    this.val = val;
    this.next = null;
  } else {
    var head = null;
    if (arguments) {
      var node;
      for (var i = 0; i < arguments[0].length; ++i) {
        if (!head) {
          head = new ListNode(arguments[0][i]);
          node = head;
        } else {
          node.next = new ListNode(arguments[0][i]);
          node = node.next;
        }
      }
    }
    return head;
  };
}

module.exports = ListNode;

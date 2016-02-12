var ListNode = require("./ListNode");

function revRecurse(head, next) {
  if (!head)
    return null;
 
  if (next === null)
    return head;

  if (next === undefined) {
    next = head.next;
    head.next = null;
  } else {
    var temp = next.next;
    next.next = head;
    head = next;
    next = temp;
  }
  return revRecurse(head, next);
}

function revIter(head) {
  if (!head)
    return null;

  var next, result;
  while (head !== null) {
    next = head.next; 
    if (!result) {
      result = head
      head = head.next;
      result.next = null;
    } else {
      head.next = result;
      result = head;
      head = next;
    }
  }
  return result;
}

(function main() {
  var head = ListNode([1,19,34,6,6,7,8]);
  var rev = revRecurse(head);

  (function recurse(node) {
    if (node !== null) {
      console.log(node.val);
      recurse(node.next);
    }
  })(rev);
})();

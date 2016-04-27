var ListNode = require("./ListNode");

var addTwoNumbers = function(l1, l2) {
  var node = null,
      head = null;
  var carry = 0,
      val = 0;
  while (l1 || l2) {
    if (!l1) {
      node.next = l2;
      node = node.next;
      val = node.val + carry;
      node.val = val % 10;
      l2 = l2.next;
    } else if (!l2) {
      node.next = l1;
      node = node.next;
      val = node.val + carry;
      node.val = val % 10;
      l1 = l1.next;
    } else {
      val = l1.val + l2.val + carry;
      if (!node) {
        node = new ListNode(val % 10);
        head = node;
      } else {
        node.next = new ListNode(val % 10);
        node = node.next;
      }

      l1 = l1.next;
      l2 = l2.next;
    }
    carry = parseInt(val / 10);
  }
  if (carry > 0)
    node.next = new ListNode(carry);

  return head;
};

(function main() {
  var l1 = ListNode([6,2,5]);
  var l2 = ListNode([4,5,6,9,3]);

  var result = addTwoNumbers(l1, l2);
  while (result) {
    console.log(result.val);
    result = result.next;
  }
})();

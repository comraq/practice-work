var ListNode = require("./ListNode");

var deleteNode = function(node) {
  if (!node)
    return null;

  if (node.next) {
    node.val = node.next.val;
    if (!node.next.next)
      node.next = null;
    else
      deleteNode(node.next);
  }
};

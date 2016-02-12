var ListNode = require("./ListNode"),
    TreeNode = require("./TreeNode");

function sortedListToBST(head) {
  /* leetcode has an extradordinarily long test case to destroy the
     recursive version of converting from Linked-List to Array */
  var list = [];
  while (head !== null) {
    list.push(head.val);
    head = head.next;
  }
  return list;
}

function arrToBST(list, low, high) {
  if (low >= high) 
    return null;

  var mid = parseInt((high + low) / 2),
      root = new TreeNode(list[mid]);

  root.left = arrToBST(list, low, mid);
  root.right = arrToBST(list, mid + 1, high);
  return root;
}

function inOrder(root, spaces) {
  if (root === null)
    return;

  spaces = (spaces === undefined)? "" : spaces + "  ";
  inOrder(root.left, spaces);
  console.log(spaces + root.val);
  inOrder(root.right, spaces);
}

(function main() {
  var a = ListNode([1, 2, 3, 4, 5, 6]);
  var arr = sortedListToBST(a);
  console.log(arr);
  inOrder(arrToBST(arr, 0, arr.length));
})();

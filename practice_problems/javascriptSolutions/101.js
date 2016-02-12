var TreeNode = require("./TreeNode");

var isSymmetric = function(root) {
  if (!root)
    return true

  return (function check(lNode, rNode) {
    if (lNode === null && rNode === null)
      return true;
    else if (lNode === null || rNode === null)
      return false;
    else if (lNode.val != rNode.val)
      return false;

    if (!check(lNode.left, rNode.right))
      return false;

    return check(lNode.right, rNode.left);
  })(root, root);
};



(function main() {
  var tree = TreeNode([1,2,2,null,null,null,null,4,null,5,null,null,6]);

  inOrder(tree);
  console.log(isSymmetric(tree));
})();

function inOrder(root, spaces) {
  if (root === null)
    return;

  spaces = (spaces === undefined)? "" : spaces + "  ";
  inOrder(root.right, spaces);
  console.log(spaces + root.val);
  inOrder(root.left, spaces);
}

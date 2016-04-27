var TreeNode = require("./TreeNode");

var levelOrder = function(root) {
  if (!root)
    return [];

  var result = [];
  (function levelOrder(node, level) {
    if (!node)
      return;

    if (level >= result.length)
      result.push([]);

    result[level].push(node.val);
    levelOrder(node.left, level + 1);
    levelOrder(node.right, level + 1);
  })(root, 0);
  return result;
};

(function main() {
  var root = TreeNode([1,2,null,3,null,4,null,5])
  //var root = TreeNode([1,2,"w",3,"x",4,"c",5])
  inOrder(root);
  console.log(levelOrder(root));
})();

function inOrder(node, spaces) {
  if (!node)
    return;

  spaces = (spaces !== undefined)? spaces + "  " : "";
  // Here, right recurse is called before left recurse to correctly print
  // a tree rotated 90 degrees left
  inOrder(node.right, spaces);
  console.log(spaces + node.val);
  inOrder(node.left, spaces);
}

"use strict"
/**
 * Exports a TreeNode constructor
 *
 * If not used as a constructor, TreeNode takes an array as arguments,
 * treats the arguments as the node value in the sequence of BFS, converting
 * it to a binary-tree and returning the root
 *
 * Note: The array is permitted to have null values which are treated as
 * null leaves, thus allowing for a non-complete tree
 */
function TreeNode(val) {
  if (this instanceof TreeNode) {
    this.val = val;
    this.left = this.right = null;
  } else {
    if (arguments)
      return toTree(arguments[0]);
    else
      return null;
  }
}

module.exports = TreeNode;

function toTree(list) {
  if (list.length <= 0)
    return null;

  var root = null,
      node = root, 
      left = root, 
      right = root;
  var nodes = [];
  for (var i = 0; i < list.length; ++i) {
    if (list[i] !== null) {
      if (node === null) {
        node = new TreeNode(list[i]);
        left = node;
        right = node;
        root = node;
      } else {
        if (node === left) {
          node.left = new TreeNode(list[i]);
          left = node.left;
        } else if (node === right) {
          node.right = new TreeNode(list[i]);
          right = node.right;
        } else {
          if (nodes.length > 0) {
            node = nodes.shift();
            nodes.push(left);
            nodes.push(right);
          } else
            node = left;

          node.left = new TreeNode(list[i]);
          left = node.left;
          right = node;
        }
      }
    } else {
      if (node !== left || node !== right) {
        if (node === right) {
          if (nodes.length > 0) {
            node = nodes.shift();
            nodes.push(left);
            left = node;
          } else
            node = left;

          right = node;
        } else if (node === left) {
          if (nodes.length > 0) {
            node = nodes.shift();
            nodes.push(right);
            right = node;
          } else
            node = right;

          left = node;
        } else {
          nodes.push(right);
          node = left;
          right = node;
        }
      }
    }
  }
  return root;
}

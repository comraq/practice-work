class IntBinaryTree {
  int i;
  IntBinaryTree left;
  IntBinaryTree right;

  IntBinaryTree(int _i, IntBinaryTree _left, IntBinaryTree _right) {
    i = _i;
    left = _left;
    right = _right;
  }

  int sumAll() {
    int ans = this.i;
    if (left != null)
      ans = ans + left.sumAll();

    if (right != null)
      ans = ans + right.sumAll();

    return ans;
  }

  // assumes array has at least one element
  static int maxArray(int[] array) {
    int ans = array[0];
    for (int i = 0; i < array.length; i++) {
      if (array[i] > ans)
        ans = array[i];
    }
    return ans;
  }
}

package myUtils;

import java.util.Arrays;

public class Heap {
  int[] arr;
  int size;

  public Heap(int[] arr) {
    this.arr = Arrays.copyOf(arr, arr.length);
    this.size = this.arr.length;
  }

  public void maxHeapify(int i) {
    int curr = i;
    int left = 2 * curr + 1;
    int right = 2 * curr + 2;

    if (left < size && arr[left] > arr[curr])
      curr = left;
    if (right < size && arr[right] > arr[curr])
      curr = right;
    if (curr != i) {
      int temp = arr[i];
      arr[i] = arr[curr];
      arr[curr] = temp;
      maxHeapify(curr);
    }
  }

  public int extractMax() {
    if (size < 1)
      return Integer.MAX_VALUE;

    int result = arr[0];
    arr[0] = arr[--size];
    maxHeapify(0);
    return result;
  }
}

import java.util.*;

import myUtils.*;

public class Q215 {
  public int findKthLargest(int[] nums, int k) {
    if (nums.length < 1)
      return Integer.MAX_VALUE;

    Heap heap = new Heap(nums);
    for (int i = heap.size / 2; i >= 0; --i)
      heap.maxHeapify(i);

    while(--k > 0)
      heap.extractMax();
    
    return heap.arr[0];
  }


  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    int[] a = {198,-3,38,0,1,-1,3,2,0};

    int n = 7;
    int[][] testCase = {{1,0},{1,2},{1,3},{2,4},{0,5},{4,6}};
    Q215 sol = new Q215();
  }
}

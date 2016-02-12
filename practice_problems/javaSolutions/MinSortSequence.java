import java.util.*;

public class MinSortSequence {
  public List<Integer> findMinRange(int[] input) {
    List<Integer> result = new ArrayList<>();
    if (input.length < 2)
      return result;

    int maxIndex = input.length - 1;
    int maxNum = Integer.MIN_VALUE;
    int currMin = Integer.MAX_VALUE;
    for (int i = input.length - 1; i >= 0; --i) {
      if (input[i] > currMin && input[i] > maxNum) {
        maxNum = input[i];
        maxIndex = i;
      }
      currMin = (input[i] < currMin)? input[i] : currMin;
    }

    currMin = Integer.MAX_VALUE;
    for (int i = maxIndex + 1; i < input.length; ++i)
      currMin = (input[i] < currMin)? input[i] : currMin;
 
    int start = -1, end = -1;
    int low = 0, high = input.length - 1;
    while (low < input.length && high >= 0
           && (start == -1 || end == -1)) {
      if (start == - 1 && input[low] > currMin)
        start = low;

      if (end == - 1 && input[high] < maxNum)
        end = high;

      ++low;
      --high;
    }

    result.add(start);
    result.add(end);
    return result;
  }

  public static void main(String[] args) {
    MinSortSequence sol = new MinSortSequence();

    int[][] testCase = {{1,2,4,7,10,11,7,12,6,7,16,18,19},
                        {1,2},
                        {},
                        {1,2,3,4,7,6,8,9},
                        {1,2,3,4,5,6,0}};

    for (int i = 0; i < testCase.length; ++i) {
      System.out.println(Arrays.toString(testCase[i]));
      System.out.println(sol.findMinRange(testCase[i]) + "\n");
    }
  }
}

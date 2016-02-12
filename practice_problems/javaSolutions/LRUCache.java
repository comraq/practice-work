import java.util.*;

public class LRUCache {

  public int getCacheMiss(int[] inStream, int cacheSize) {
    if (inStream == null || inStream.length <= cacheSize)
      return 0;

    int result = 0, hit = 0;
    Map<Integer, Integer> table = new HashMap<>();
    for (int i = 0; i < inStream.length; ++i) {
      int inputVal = inStream[i];
      if (!table.containsKey(inputVal)) {
        if (table.size() >= cacheSize)
          ++result;
      } else {
        if (i - table.get(inputVal) - hit <= cacheSize)
          ++hit;
        else
          ++result;
      }
      table.put(inputVal, i);
    }
    return result;
  }

  public static void main(String[] args) {
    LRUCache sol = new LRUCache();
    int[][] testCases = {{0,1,2,3,4,5,6,0,0,1,6,5,4,0,8,7},
                         {},
                         {0,-1,-3,-5,12,2,0,4,0,4,-5,2,12,4}};
    for (int i = 0; i < testCases.length; ++i) {
      System.out.println(Arrays.toString(testCases[i]));
      System.out.println("Cache misses: " + sol.getCacheMiss(testCases[i], 5));
    }
  }
}

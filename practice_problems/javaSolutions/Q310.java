import java.util.*;

public class Q310 {
  public List<Integer> findMinHeightTrees(int n, int[][] edges) {
    List<Integer> result = new ArrayList<>();
    if (n < 0)
      return result;
    else if (n < 3) {
      for (int i = 0; i < n; ++i)
        result.add(i);

      return result;
    }

    List<List<Integer>> adjList = new ArrayList<>();
    for (int i = 0; i < n; ++i)
      adjList.add(new ArrayList<Integer>());

    for (int i = 0; i < edges.length; ++i) {
      int nodeA = edges[i][0];
      int nodeB = edges[i][1];

      adjList.get(nodeA).add(nodeB);
      adjList.get(nodeB).add(nodeA);
    }

    Deque<Integer> queue = new ArrayDeque<>();
    for (int i = 0; i < n; ++i)
      if (adjList.get(i).size() == 1)
        queue.add(i);

    int remain = adjList.size();
    while (remain > 2) {
      int count = queue.size();
      while(count > 0) {
        Integer leaf = queue.removeFirst();
        Integer neighbour = adjList.get(leaf).remove(0);
      
        adjList.get(neighbour).remove(leaf);
        if (adjList.get(neighbour).size() == 1)
          queue.add(neighbour);

        --count;
        --remain;
      } 
    }
    
    while (!queue.isEmpty())
      result.add(queue.remove());

    return result;
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
    Q310 sol = new Q310();
    System.out.println(sol.findMinHeightTrees(n, testCase));
  }
}

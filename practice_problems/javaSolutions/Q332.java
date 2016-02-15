import java.util.*;

import myUtils.*;

public class Q332 {
  public List<String> findItinerary(String[][] tickets) {
    if (tickets == null || tickets.length < 1)
      return null;

    Map<String, List<String>> table = new HashMap<>();
    for (int i = 0; i < tickets.length; ++i) {
      if (tickets[i].length != 2) return null;
   
      String from = tickets[i][0], to = tickets[i][1];
      if (table.containsKey(from))
        table.get(from).add(to);
      else
        table.put(from, new ArrayList<String>(Arrays.asList(to)));
    }
    for (String key: table.keySet())
      Collections.sort(table.get(key));

    List<String> result = new ArrayList<>();
    if (!findRecurse(result, table, "JFK"))
      return null;

    return result;
  }

  private boolean findRecurse(List<String> result,
                              Map<String, List<String>> table, String from) {
    boolean done = true;
    for (String key: table.keySet()) {
      if (table.get(key).size() > 0) {
        done = false;
        break;
      }
    }
    if (done) {
      result.add(from);
      return true;
    }
    if (!table.containsKey(from)) return false;

    int size = table.get(from).size();
    for (int i = 0; i < size; ++i) {
      String dest = table.get(from).remove(i);
      if (findRecurse(result, table, dest)) {
        result.add(0, from);
        return true;
      } else {
        result.clear();
        table.get(from).add(i, dest);
      }
    }
    return false;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    int[] a = {198,-3,38,0,1,-1,3,2,0};
    Q332 sol = new Q332();
    String[] s = {"asdf", ".,wr23", "192inf-", "/wep23rn"};
    System.out.println(Arrays.toString(s));
    Arrays.sort(s);
    s[2] = null;
    System.out.println(Arrays.toString(s));
  }
}

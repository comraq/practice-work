import java.util.*;

public class Q017 {
  public List<String> letterCombinations(String digits) {
    if (digits == null || digits.length() < 1
        || digits.matches(".*\\D|.*0|.*1"))
      return new ArrayList<String>();
 
    Map<String, String> table = new HashMap<>();
    table.put("2", "abc");
    table.put("3", "def");
    table.put("4", "ghi");
    table.put("5", "jkl");
    table.put("6", "mno");
    table.put("7", "pqrs");
    table.put("8", "tuv");
    table.put("9", "wxyz");

    return getCombinations(table, digits);
  }

  private List<String> getCombinations(Map<String, String> table,
                                       String digits) {
    List<String> result = new ArrayList<>();
    if (digits.length() == 1) {
      String letters = table.get(digits);
      for (int i = 0; i < letters.length(); ++i)
        result.add(letters.substring(i, i + 1));
    } else {
      List<String> subResults = getCombinations(table, digits.substring(1));
      String letters = table.get(digits.substring(0,1));
      for (String subResult: subResults) {
        for (Character c: letters.toCharArray())
          result.add(c + subResult);
      }
    }
    return result;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q017 sol = new Q017();
    System.out.println(sol.letterCombinations("32"));
    System.out.println(sol.letterCombinations("293850"));
    System.out.println(sol.letterCombinations("23-"));
    String[] test = {"23-", "293850", "234", "as;lfweo"};
    for (int i = 0; i < test.length; ++i)
      System.out.println(test[i].matches(".*\\D|.*0|.*1"));
  }
}

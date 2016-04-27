import java.util.*;

public class Q155 {
  private Stack<Integer> stack = new Stack<>();
  private Stack<Integer> minStack = new Stack<>();

  public void push(int x) {
    stack.push(x);
    if (minStack.empty() || x <= minStack.peek())
      minStack.push(x);
  }

  public void pop() {
    if (!stack.empty()) {
      if (minStack.peek().equals(stack.pop()))
        minStack.pop();
    }
  }

  public int top() {
    return stack.peek();
  }

  public int getMin() {
    return minStack.peek();
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q155 sol = new Q155();
    System.out.println(sol.letterCombinations("32"));
    System.out.println(sol.letterCombinations("293850"));
    System.out.println(sol.letterCombinations("23-"));
    String[] test = {"23-", "293850", "234", "as;lfweo"};
    for (int i = 0; i < test.length; ++i)
      System.out.println(test[i].matches(".*\\D|.*0|.*1"));
  }
}

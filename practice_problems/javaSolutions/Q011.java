import java.util.*;

public class Q011 {
  public int maxArea(int[] height) {
    if (height == null || height.length < 2)
      return 0;

    int area = 0;
    int left = 0, right = height.length - 1;
    while (left < right) {
      int high, temp;
      if (height[left] < height[right]) {
        temp = height[left] * (right - left);
        ++left;
      } else {
        temp = height[right] * (right - left);
        --right;
      }
      area = (temp > area)? temp : area;
    }
    return area;
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    Q011 sol = new Q011();
  }
}

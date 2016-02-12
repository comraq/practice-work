import java.util.*;

public class Q004 {
  public double findMedianSortedArrays(int[] nums1, int[] nums2) {
    System.out.println(nums1.length);

    return medianRecurse(nums1, nums2, 0, nums1.length, 0, nums2.length);
  }

  private double medianRecurse(int[] arrA, int[] arrB, int aLow, int aHigh,
int bLow, int bHigh) {
    if (aLow >= aHigh && bLow >= bHigh)
      return (arrA[aLow] + arrB[bLow]) / 2.0

    int aMid = (aHigh + aLow) / 2;
    int bMid = (bHigh + bLow) / 2;

    if (arrA[aMid] > arrB[bMid]) {
      aHigh = aMid;
      bLow = bMid + 1;
    } else {
      aLow = aMid + 1;
      bHigh = bMid;
    }

    return medianRecurse(arrA, arrB, aLow, aHigh, bLow, bHigh);
  }

  public static void main(String[] args) {
    List<String> ex = new ArrayList<>(Arrays.asList(
                      "eeefffffasdf",
                      "effasde",
                      "e",
                      "wermm31k4t",  
                      "ehsdf"));

    int[] a = {1,2,3};
    int[] b = {1,1212,4,5};
    Q004 sol = new Q004();
    System.out.println(sol.findMedianSortedArrays(a, b));
  }
}

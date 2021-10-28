class Solution {
  def diagonalDifference(arr: Array[Array[Int]]): Int = {
    // Write your code here
    var leftDiagonalSummation = 0
    var rightDiagonalSummation = 0
    val bound = arr.size - 1
    for (i <- 0 to bound) {
      leftDiagonalSummation = leftDiagonalSummation + arr(i)(i)
      rightDiagonalSummation = rightDiagonalSummation + arr(i)(bound - i)
    }
    abs(leftDiagonalSummation - rightDiagonalSummation)
  }
}

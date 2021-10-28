class Solution {
  def getNextJump(x: Int, v: Int): Int = {
    x + v
  }
  def getJumpForLargestX(x1: Int, v1: Int, x2: Int, v2: Int): (Int, Int) = {
    if (x1 > x2) (x1, getNextJump(x2, v2))
    else (getNextJump(x1, v1), x2)
  }
  def kangaroo(x1: Int, v1: Int, x2: Int, v2: Int): String = {
    var difference: Int = Int.MaxValue
    var oldDifference: Int = difference
    var currentX1: Int = x1
    var currentX2: Int = x2
    if (x2 > x1) {
      // calculate difference as x1 begins at greater distance
      return kangaroo(x2, v2, x1, v1)
    }
    while (true) {
      currentX1 = getNextJump(currentX1, v1)
      currentX2 = getNextJump(currentX2, v2)
      difference = currentX1 - currentX2
      println(difference)
      if (difference == 0) return "YES"
      if (difference < 0) return "NO"
      if (difference > oldDifference) return "NO"
      oldDifference = difference
    }
    "NO"
  }
  def interativeKangaroo(x1: Int, v1: Int, x2: Int, v2: Int): String = {
    // Cooler solution
    var differences = Set[Int]()
    var currentX1: Int = x1
    var currentX2: Int = x2
    while (true) {
      val xs =
        getJumpForLargestX(currentX1, v1, currentX2, v2)
      currentX1 = xs._1
      currentX2 = xs._2
      println(currentX1, currentX2)
      val difference = currentX1 - currentX2
      if (difference == 0) return "YES"
      else if (differences.contains(difference)) return "NO"
      else differences = differences + difference
    }
    "NO"
  }
}

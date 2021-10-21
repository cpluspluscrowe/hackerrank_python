class Solution {
  def getValidFactors(start: Int, maxValidFactor: Int): Array[Int] = {
    val arrayLength: Int = maxValidFactor / start
    var factors: Array[Int] = new Array[Int](0);
    for (factor <- start to maxValidFactor by start) {
      factors = factors :+ factor
    }
    return factors
  }

  def isFactorValidMod(factor: Int, mustModInto: Array[Int]): Boolean = {
    for (mustBeModded <- mustModInto) {
      if (mustBeModded % factor != 0) {
        return false
      }
    }
    return true
  }

  def isNumberDivisibleByFactors(
      number: Int,
      originalFactors: Array[Int]
  ): Boolean = {
    for (factor <- originalFactors) {
      if (number % factor != 0) return false
    }
    return true
  }

  def getTotalX(a: Array[Int], b: Array[Int]): Int = {
    val minValidFactor: Int = a.max
    val maxValidFactor: Int = b.min
    val firstElement: Int = a(0)
    val factors = a
      .map(getValidFactors(_, maxValidFactor))
      .flatten
      .filter(_ >= minValidFactor)
      .filter(isFactorValidMod(_, b))
      .toSet
      .toArray
    val allFactorsShouldBeMods = factors
      .filter(isNumberDivisibleByFactors(_, a))
    return allFactorsShouldBeMods.length
  }
}

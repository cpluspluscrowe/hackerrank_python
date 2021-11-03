class LeaderBoard {
  def insertIntoArray(
      valueToAdd: Int,
      indexToAdd: Int,
      array: Array[Int]
  ): Array[Int] = {
    // do not insert if equal to current element
    if (indexToAdd < array.size) {
      if (valueToAdd == array(indexToAdd)) {
        return array
      }
    }
    array.slice(0, indexToAdd) ++ Array(valueToAdd) ++ array.slice(
      indexToAdd,
      array.size
    )
  }

  def getInsertionIndex(valueToAdd: Int, sortedArray: Array[Int]): (Int) = {
    // edge case
    if (valueToAdd < sortedArray(sortedArray.size - 1)) {
      return sortedArray.size
    }
    // main logic
    var high = sortedArray.size - 1
    var low = 0
    var mid: Int = 0
    var findLargestValueSmallerThanElement = high
    while (low <= high) {
      mid = low + (high - low) / 2
      val value = sortedArray(mid)
      if (value.equals(valueToAdd)) {
        findLargestValueSmallerThanElement = mid
        // break out
        low = Int.MaxValue
        high = Int.MinValue
      } else if (valueToAdd > value) {
        // element is greater
        findLargestValueSmallerThanElement = mid
        high = mid - 1
      } else {
        low = mid + 1
      }
    }
    return findLargestValueSmallerThanElement
  }

  def insertElementAndGetRank(
      element: Int,
      scores: Array[Int]
  ): (Int, Array[Int]) = {
    val insertionIndex = getInsertionIndex(element, scores)
    val updatedScores = insertIntoArray(element, insertionIndex, scores)
    return (insertionIndex, updatedScores)
  }

  def addNewScore(
      score: Int,
      addHere: Array[Int],
      previousScores: Set[Int]
  ): Array[Int] = {
    if (previousScores.contains(score)) {
      return addHere
    }
    addHere :+ score
  }

  def setScores(scores: Array[Int]): Array[Int] = {
    var previousScores = Set[Int]()
    var scoresToReturn = Array[Int]()
    for (score <- scores) {
      scoresToReturn = addNewScore(score, scoresToReturn, previousScores)
      previousScores += score
    }
    scoresToReturn
  }

  def climbingLeaderboard(
      ranked: Array[Int],
      newScores: Array[Int]
  ): Array[Int] = {
    var scores = setScores(ranked)
    var rankings = Array[Int]()
    for (score <- newScores) {
      val updatedScores = insertElementAndGetRank(score, scores)
      rankings = rankings :+ updatedScores._1 + 1
      scores = updatedScores._2
    }
    rankings
  }
}

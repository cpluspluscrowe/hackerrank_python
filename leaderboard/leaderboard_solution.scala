import scala.collection.SortedMap
import scala.collection.SortedSet
import java.util.Collections
object Result {

    /*
     * Complete the 'climbingLeaderboard' function below.
     *
     * The function is expected to return an INTEGER_ARRAY.
     * The function accepts following parameters:
     *  1. INTEGER_ARRAY ranked
     *  2. INTEGER_ARRAY player
     */

  def getPrecedingScore2(newScore: Int, sortedKeys: SortedSet[Int]): Int = {
    var lowestSoFar = sortedKeys.head    
    for(key <- sortedKeys){
      if(key <= newScore){
        if(key > lowestSoFar){
          lowestSoFar = key
        }
      }
    }
    lowestSoFar
  }

  def getPrecedingScore(newScore: Int, sortedKeys: Array[Int]): Int = {
    var high = sortedKeys.size - 1
    var low = 0
    var lowestSoFar = sortedKeys.head
    while(low <= high && low >= 0 && high < sortedKeys.size){
      var mid = low + (high - low) / 2
      val value = sortedKeys(mid)
      if(value.equals(newScore)){
        return value
      }
      if(newScore > value){
        lowestSoFar = value
        low = mid + 1
      }else{
        high = mid - 1
      }
    }
    lowestSoFar
  }
  def processScores(ranked: Array[Int]): SortedMap[Int, Int] = {
    var sortedScores = SortedMap[Int, Int]()
    var currentRank = 0
    for(rank <- ranked){
      if(!sortedScores.contains(rank)){
        currentRank = currentRank + 1
      }
      sortedScores = sortedScores + (rank -> currentRank)
    }
  sortedScores
  }
  def getAccumulation(newScore: Int, accumulations: Array[Int]): Int = {
    var scoreDecrement = 0
    for(accumulation <- accumulations){
      if(newScore < accumulation){
        scoreDecrement = scoreDecrement + 1
      }
    }
    scoreDecrement
  }
  def getRank(newScore: Int, lowerScore: Int, lowerRank: Int): Int = {
    if(newScore.equals(lowerScore)) return lowerRank
    if(newScore > lowerScore) return lowerRank
    if(newScore < lowerScore) return lowerRank + 1
    throw new Exception("No rank found")
  }
  def getPlayerScoreRank(newScore: Int, sortedScores: SortedMap[Int, Int], accumulations: Array[Int]): (Int, SortedMap[Int, Int], Array[Int]) = {
    val decrementFinalRankBy = getAccumulation(newScore, accumulations)
    val lowerScoreBy = getAccumulation(newScore, accumulations)
    val existingLowerOrEqualScore: Int = getPrecedingScore2(newScore, sortedScores.keySet)
    val oldRank = sortedScores(existingLowerOrEqualScore)
    val newScoreRank = getRank(newScore, existingLowerOrEqualScore, oldRank)
    val newTreeMap = sortedScores + (newScore -> newScoreRank)
    return (newScoreRank + decrementFinalRankBy, newTreeMap, accumulations :+ newScore)
  }
    def climbingLeaderboard(ranked: Array[Int], player: Array[Int]): Array[Int] = {
      var sortedScores = processScores(ranked)
      var accumulations = Array[Int]()
      var answers = Array[Int]()
      for(playerScoreIndex <- 0 until player.size){
        val playerScore = player(playerScoreIndex)
        val currentPlayerRankData = getPlayerScoreRank(playerScore, sortedScores, accumulations)
        val rank = currentPlayerRankData._1
        answers = answers :+ rank
        sortedScores = currentPlayerRankData._2
        accumulations = currentPlayerRankData._3
      }
      answers
    }

}




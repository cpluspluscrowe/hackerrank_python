import util.control.Breaks._
class CheckStringViabilityViaAppendAndDelete{
  def getLengthOfInitialStringMatch(s: String, t: String): Int = {
    val sChars = s.toCharArray
    val tChars = t.toCharArray
    var count = 0
    breakable {
    for(i <- 0 until Math.min(sChars.size, tChars.size)){
      if(sChars(i).equals(tChars(i))){
        count = count + 1
      }else{
        break
      }
    }}
    return count
  }
  def getRequiredDeletionOperations(s: String, t: String): Int = {
    val frontMatchCount = getLengthOfInitialStringMatch(s, t)
    val initialDeletionCount = s.size - frontMatchCount
    val remainingToAppend = t.size - frontMatchCount
    initialDeletionCount + remainingToAppend
  }
  def getRequiredOperationsCount(s: String, t: String): (Int, Int) = {
    val frontMatchCount = getLengthOfInitialStringMatch(s, t)
    val initialDeletionCount = s.size - frontMatchCount
    val remainingToAppend = t.size - frontMatchCount
    (initialDeletionCount, remainingToAppend)
  }

  def appendAndDelete(s: String, t: String, k: Int): String = {
    val (deletionCount, appendCount) = getRequiredOperationsCount(s,t)
    val operationCount = deletionCount + appendCount    
    val trivialAddDeletions = k - operationCount
    if(operationCount > k) return "No"
    if(trivialAddDeletions > t.size) return "Yes"
    if(trivialAddDeletions % 2 == 0 || deletionCount == s.size) return "Yes"
    if(operationCount == k || trivialAddDeletions % 2 == 0){
      "Yes"
    }else{
      "No"
    }
  }
}

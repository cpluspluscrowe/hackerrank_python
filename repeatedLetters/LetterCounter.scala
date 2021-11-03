class LetterCounter {
  def countAs(baseString: String): Long = {
    var count: Long = 0
    for (letter <- baseString) {
      if (letter == 'a') {
        count = count + 1
      }
    }
    return count
  }
  def repeatedString(s: String, n: Long): Long = {
    val aCount = countAs(s)
    val repeatCount = n / s.size
    val modSize: Long = n % s.size
    val aModCount = countAs(s.substring(0, modSize.toInt))
    aModCount + aCount * repeatCount
  }
}

import scala.util.control.Breaks._
class FinderForNextLargestWordViaLetterSwapping {
  def isSwapPossible(letters: Array[Char]): Boolean = {
    var currentValue = letters(0).toInt
    for(letter <- letters.slice(1,letters.size)){
      val letterValue = letter.toInt
      if(letterValue > currentValue){
        return true
      }
      currentValue = letterValue
    }
    return false
  }
  def findViableSwap(letters: Array[Char]): (Int, Int) = {
    for(i <- letters.length - 1 to 0 by -1){
      for(j <-  i - 1 to 0 by -1){
        if(letters(i) > letters(j)){
          return (i, j)
        }
      }
    }
    throw new Exception("No viable swap indexes")
  }
  def biggerIsGreater(w: String): String = {
    // goal is to move a larger letter forward in the string
    var wordArray = w.toCharArray()
    var original = wordArray
    if(!isSwapPossible(wordArray)) return "no answer"
    breakable{
    while(isSwapPossible(wordArray)){
      val (latterIndex, firstIndex) = findViableSwap(wordArray)
      val newWordArray = wordArray.slice(0,firstIndex) ++ Array[Char](wordArray(latterIndex)) ++ wordArray.slice(firstIndex + 1, latterIndex) ++ Array[Char](wordArray(firstIndex)) ++ wordArray.slice(latterIndex + 1, wordArray.size)
      val newWord = String.valueOf(newWordArray)
      val oldWord = String.valueOf(wordArray)
      if(newWord > w && newWord < oldWord){
        wordArray = newWordArray
      }else{
        break
      }
    }
    }
    String.valueOf(wordArray)
  }
}

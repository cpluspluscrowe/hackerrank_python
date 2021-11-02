import scala.collection.immutable.Queue

class Candies {
  def rotateDeque(n: Int, deque: Queue[Int]): Queue[Int] = {
    if (n <= 0) return deque
    var ad = deque
    for (i <- 0 until n) {
      val removed = ad.dequeue
      val item = removed._1
      ad = removed._2
      ad = ad.enqueue(item)
    }
    ad
  }
  def saveThePrisoner(n: Int, m: Int, s: Int): Int = {
    val accountForMultipleRounds = m % n
    var ad = Queue[Int]()
    for (i <- 0 until n) {
      ad = ad :+ i
    }
    var rotateToStart = rotateDeque(s - 1, ad)
    var result: Int = rotateToStart.head
    for (candy <- 0 until accountForMultipleRounds) {
      val removed = rotateToStart.dequeue
      result = removed._1
      rotateToStart = removed._2
    }
    result + 1
  }
}

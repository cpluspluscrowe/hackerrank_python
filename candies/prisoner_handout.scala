import scala.collection.immutable.Queue

class Candies {
  def getLast(current: Int, size: Int): Int = {
    if(current == 1){
      return size
    }else{
      return current - 1
    }
  }
  def feed(current: Int, size: Int): Int = {
    if(current == size){
      return 1
    }else{
      return current + 1
    }
  }
  def feed2(queue: Queue[Int]): Queue[Int] = {
    val removed = queue.dequeue
    val item = removed._1
    val updatedQueue = removed._2
    updatedQueue.enqueue(item)
  }
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
  def fillQueue(n: Int): Queue[Int] = {
    var ad = Queue[Int]()
    for (i <- 0 until n) {
      ad = ad :+ i
    }
    ad
  }
  def saveThePrisoner(n: Int, m: Int, s: Int): Int = {
    val rotations = (s - 1 + m) % n
    getLast(rotations, n)
  }
  def saveThePrisoner3(n: Int, m: Int, s: Int): Int = {
    var queue = fillQueue(n)
    val rotations = (s - 1 + m) % n
    for(i <- 0 until rotations){
      queue = feed(queue)
    }
    queue.last + 1
  }
  def saveThePrisoner2(n: Int, m: Int, s: Int): Int = {
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

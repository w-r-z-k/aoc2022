object Day6 extends App {

  import scala.io.Source
  import scala.collection.mutable

  val f = Source.fromFile("../../../data/Day6.txt")

  val signal = f.getLines().toList.head

  class FiniteQueue[A](n: Int) extends mutable.Queue[A] {// only n spots in FIFO Q
    override def enqueue(elem: A): this.type = {
      this += elem
      while(super.size > n) super.dequeue()
      this
    }
  }

  def find(signal: String, len: Int): Int = {
    val q = new FiniteQueue[Char](len).enqueueAll(signal.take(len).toList) // start with 1st 4 chars in FIFO Queue

    signal.drop(len).collect(x => { // for each char ... collect a tuple (Set(Queue(4 chars)), index)
      q.enqueue(x).toSet // add 1 char to the 4 char FIFO Q, and create a set from the Q
    }).toList.zipWithIndex.filter(x => x._1.size == len).head._2 + 1 + len // get index of 1st 4 char set
  }
   
  println("answer1=" + find(signal, 4))
  println("answer2=" + find(signal, 14))
}

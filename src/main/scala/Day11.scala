object Day11 extends App {

  import scala.io.Source
  import scala.collection.mutable.Queue

  val f = Source.fromFile("../../../data/Day11.txt")

  type Worry = BigInt

  val lines = f.getLines().toList

  case class Monkey(
    val id: Int,
    val items: Queue[Worry],
    val op: Char, 
    val opEnd: Int,
    val div: Int,
    val ifTrue: Int, 
    val ifFalse: Int ) {

    override def toString() = {
       s"Monkey $id: items=$items op=$op opEnd=$opEnd div=$div ifTrue=$ifTrue ifFalse=$ifFalse"
    }
  }

  object Monkey {

    def read(defn: List[String]): Monkey = {
  
      val s"Monkey $number:" = defn(0) : @unchecked
      val id = number.toInt
      val items: Queue[Worry] = Queue().enqueueAll(defn(1).drop(18).split(", ").map(BigInt(_)))
      val op = defn(2)(23)
      val opEnd0 = defn(2).drop(25)
      val opEnd = if (opEnd0 == "old") Int.MaxValue else opEnd0.toInt
      val div = defn(3).drop(21).toInt
      val ifTrue = defn(4).drop(29).toInt
      val ifFalse = defn(5).drop(30).toInt
  
      Monkey(id, items, op, opEnd, div, ifTrue, ifFalse)
    }
  
    def readAll(lines: List[String]) = (for (m <- lines.grouped(7)) yield read(m)).toArray
  }

  def run(rounds: Int, reduction: Worry => Worry): Long = {
    var inspect = new Array[Long](monkeys.size)

    for (r <- 0 until rounds) {
      for (m <- monkeys) {
        while (!m.items.isEmpty) {
          inspect.update(m.id, inspect(m.id) + 1)
          val item = m.items.dequeue()
          val item1 = reduction((m.op match {
            case '+' => item + (if(m.opEnd == Int.MaxValue) item else m.opEnd)
            case '*' => item * (if(m.opEnd == Int.MaxValue) item else m.opEnd)
          }))

          if (item1 % m.div == 0)
            monkeys(m.ifTrue).items.enqueue(item1)
          else
            monkeys(m.ifFalse).items.enqueue(item1)
        }
      }
    }
    inspect.toList.sorted(Ordering.Long.reverse).take(2).foldLeft(1L)(_ * _)
  }

  var monkeys = Monkey.readAll(lines)

  println("answer1=" + run(20, _ / 3))

  monkeys = Monkey.readAll(lines)

  println("answer2=" + run(10000, _ % monkeys.map(_.div).product))
}

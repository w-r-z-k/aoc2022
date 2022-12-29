object Day10 extends App {

  import scala.io.Source

  val f = Source.fromFile("../../../data/Day10.txt")

  val input = f.getLines().toList.map {x => x match { case "noop" => (x, 0) 
                                                      case  addx  => ("addx",x.drop(5).toInt) } }

  def check(op: String, v: Int): Unit = {// check & tally the signal strength
    if ((cycle - 20) % 40 == 0)
      total = total + (cycle * X)
    cycle = cycle + 1
  }

  def draw(op: String, v: Int): Unit = {// draw the appropriate pixel
    val pos = (cycle - 1) % 40
    if (pos == 0) println()
    print(if (pos == X || pos == (X - 1) || pos == (X + 1)) "#" else ".")
    cycle = cycle + 1
  }

  def run(f: (String, Int) => Unit) = {
    for (ins <- input) {// ins.(_1, _2) :== (op, value)
      if (ins._1 == "addx") {
        f(ins._1, ins._2)
        f(ins._1, ins._2)
        X = X + ins._2
      } else { // noop
        f(ins._1, ins._2)
      }
    }
  }

  var X = 1
  var cycle = 1
  var total = 0

  run(check)
  println("answer1=" + total)

  println("answer2=")
  X = 1
  cycle = 1
  run(draw)
  println()

}

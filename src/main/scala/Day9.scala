object Day9 extends App {

  import scala.io.Source

  val f = Source.fromFile("../../../data/Day9.txt")

  val input = f.getLines().toList

  val moves = input.map(l => (l.head, l.tail.tail.toInt))

  def sign(i: Int) = {if (i == 0) 0 else if (i < 0) -1 else 1}

  def rope(n: Int) = {
    val k = Array[(Int, Int)]().padTo(n, (0,0))

    val v = scala.collection.mutable.Set[(Int, Int)]()
    v.addOne(k(n - 1))

    moves.foreach(m => {
      for (mi <- 0 until m._2) {
        var h = m._1 match {
          case 'R' => (k(0)._1 + 1, k(0)._2)
          case 'L' => (k(0)._1 - 1, k(0)._2)
          case 'U' => (k(0)._1, k(0)._2 - 1)
          case 'D' => (k(0)._1, k(0)._2 + 1)
        }
        k.update(0, h)

        for (i <- 1 to (n - 1)) {
          var t = k(i)
          var d = (sign(h._1 - t._1), sign(h._2 - t._2))

          while (Math.abs(h._1 - t._1) > 1 || Math.abs(h._2 - t._2) > 1) {
            t = (t._1 + d._1, t._2 + d._2)
            if (h._1 == t._1) d = (0, d._2)
            else if (h._2 == t._2) d = (d._1, 0)
            if (i == (n - 1)) v.addOne(t)
          }
          k.update(i, t)
          h = t
        }
      }
    })
    v.size
  }

  println("answer1=" + rope(2))
  println("answer2=" + rope(10))
}


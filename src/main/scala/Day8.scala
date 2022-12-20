object Day8 extends App {

  // This is a truly horrible (but correct) solution.
  // The first part I did using four boolean matrices - one for each direction.
  // This would have been simple in apl :)
  // Not knowing how to represent matrices and bit arrays in Scala made the code 
  // essentially C by Scala. All horrible loops and indices.  But 'transpose' was useful.
  // Worse yet, part 2 didn't make use of the boolean matrices at all, so it 
  // was basically another (much simpler) C by Scala program.  
  // That indicated to me that part 1 would have been much simpler with the direct approach.  
  // All-in-all not much idiomatic Scala in this solution.
  // Perhaps I will revisit this solution in future.

  import scala.io.Source

  // val f = Source.fromFile("../../../data/Day8.example.txt")
  val f = Source.fromFile("../../../data/Day8.txt")

  val input = f.getLines().toList

  var nc = input(0).size
  var nr = input.size

  val forest = Array.ofDim[Byte](nr, nc)

  for (i <- (0 until nr)) {
      forest.update(i, input(i).toArray.map(x => {(x - '0').toByte}))
  }

  val west = Array.ofDim[Boolean](nr, nc)
  val east = Array.ofDim[Boolean](nr, nc)

  for (i <- (0 until nr)) {
      val r = forest(i)
      val w = west(i)
      var hw = r(0)
      w.update(0, true)
      for (j <- (1 until nc)) {
        if (r(j) > hw) {
          w(j) = true
          hw = r(j)
        }
      }
      // from the east
      val e = east(i)
      var he = r(nc - 1)
      e.update(nc - 1, true)
      for (j <- (nc - 2 until 0 by -1)) {
        if (r(j) > he) {
          e(j) = true
          he = r(j)
        }
      }
  }
  val we = Array.ofDim[Boolean](nr, nc)
  for (i <- (0 until nr)) {
    for (j <- (0 until nc)) {
      we(i)(j) = west(i)(j) | east(i)(j)
    }
  }
  val forest_t = forest.transpose
  nc = forest_t(0).size
  nr = forest_t.size
  val north = Array.ofDim[Boolean](nr, nc)
  val south = Array.ofDim[Boolean](nr, nc)
  for (i <- (0 until nr)) {
      // from the north
      val r = forest_t(i)
      val n = north(i)
      var hn = r(0)
      n.update(0, true)
      for (j <- (1 until nc)) {
        if (r(j) > hn) {
          n(j) = true
          hn = r(j)
        }
      }
      // from the south
      val s = south(i)
      var hs = r(nc - 1)
      s.update(nc - 1, true)
      for (j <- (nc - 2 until 0 by -1)) {
        if (r(j) > hs) {
          s(j) = true
          hs = r(j)
        }
      }
  }
  val ns = Array.ofDim[Boolean](nr, nc)
  for (i <- (0 until nr)) {
    for (j <- (0 until nc)) {
      ns(i)(j) = north(i)(j) | south(i)(j)
    }
  }

  val ns_t = ns.transpose

  nc = forest(0).size
  nr = forest.size
  val v = Array.ofDim[Boolean](nr, nc)
  for (i <- (0 until nr)) {
    for (j <- (0 until nc)) {
      v(i)(j) = we(i)(j) | (ns_t(i)(j))
    }
  }

  println("answer1=" + v.foldLeft(0)((x, y) => {x + y.count(_ == true)}))

  def score(r: Int, c: Int) = {
    val h = forest(r)(c)
    var i = 0
    // look up
    i = r - 1
    var n = if (i < 0 ) 0 else 1
    while (i > 0 && h > forest(i)(c)) {n = n + 1; i = i - 1}
    // look down
    i = r + 1
    var s = if (i == nr) 0 else 1
    while (i < nr - 1 && h > forest(i)(c)) {s = s + 1; i = i + 1}
    // look west
    i = c - 1
    var w = if (i < 0 ) 0 else 1
    while (i > 0 && h > forest(r)(i)) {w = w + 1; i = i - 1}
    // look east
    i = c + 1
    var e = if (i == nc) 0 else 1
    while (i < nc - 1 && h > forest(r)(i)) {e = e + 1; i = i + 1}

    n * w * s * e
  }

  var best = 0

  for (i <- (0 until nr)) {
    for (j <- (0 until nc)) {
      val current = score(i, j)
      if (current > best) best = current
    }
  }

  println("answer2=" + best)
}

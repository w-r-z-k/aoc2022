object Day14 extends App {// Scala 2.13.7

  val f = scala.io.Source.fromFile("../../../data/Day14.txt")

  // e.g. Convert lines "(498,4 -> 498,6 -> 496,6)" to a List((498,4), (498,6), (496,6))
  val input = f.getLines().toList.map(x => x.split(" -> ").map(x => x.split(',')).map(x => (x(0).toInt, x(1).toInt)))

  def drawLine(from: (Int, Int), to: (Int, Int)): Seq[(Int, Int)] = {// return a set of all points [from, to]
    if (from._1 == to._1) // vertical line
      for (y <- from._2 to to._2 by (if (from._2 < to._2) 1 else -1)) yield (from._1, y)
    else // horizontal line
      for (x <- from._1 to to._1 by (if (from._1 < to._1) 1 else -1)) yield (x, from._2)
  }

  def run(cave: Set[(Int, Int)], deepest: Int) = {// simulate
    var units = 0       // blocks of sand coming to rest
    var sand = (500, 0) // drop point for the sand blocks
    var break = false   // used for part 2 only
    var c = cave        // set of blocked points (rock or sand)
    while (!break && sand._2 < (deepest + 2)) {
      val below = (sand._1, sand._2 + 1)
      val left = (sand._1 - 1, sand._2 + 1)
      val right = (sand._1 + 1, sand._2 + 1)
      if (!c.contains(below)) {           // directly below is not blocked 
        sand = (sand._1, sand._2 + 1)     //   move down 1
      } else if (!c.contains(left)) {     // below and to the left is not blocked
        sand = (sand._1 - 1, sand._2 + 1) //   move down 1 and to the left
      } else if (!c.contains(right)) {    // below and to the right is not blocked
        sand = (sand._1 + 1, sand._2 + 1) //   move down 1 and to the right
      } else { // otherwise, sand stops here
        units = units + 1
        c = c + sand // add this sand block's rest point to the set
        if (sand == (500, 0))
          break = true // part 2: sand is backed up to the drop point
        else 
          sand = (500, 0) // reset to drop point for next sand drop
      }
    }
    units
  }
  // To collect the initial set of points representing the location of the rocks:
  //   Accumulate sets of points by drawing the lines between successive straight line points.
  //   There is a fold within a fold: the inner one for the points of an input line and the
  //   outer one for all input lines.  The set of points on an input line zipped with itself
  //   enables the fold to draw between points.
  var cave = input.foldLeft(Set[(Int, Int)]())((b, a) => b ++ a.zip(a.tail)
                  .foldLeft(Set[(Int, Int)]())((c, d) => c ++ drawLine(d._1, d._2)))

  val deepest = cave.map(_._2).max // The lowest point a rock occurs (i.e. y coord or distance down)

  println("answer1=" + run(cave, deepest))
                                // This is the additional infinite bottom rock line in part 2.
  println("answer2=" + run(cave ++ drawLine((cave.map(_._1).min - deepest - 2, deepest + 2),  // leftmost
                                            (cave.map(_._1).max + deepest + 2, deepest + 2)), // rightmost
                           deepest))
}

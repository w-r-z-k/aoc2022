object Day15 extends App {

  val input = scala.io.Source.fromFile("../../../data/Day15.txt").getLines().toList
  
  final case class Pos(x: Int, y: Int)
  
  val r = "Sensor at x=(\\d+), y=(\\d+): closest beacon is at x=(-*\\d+), y=(\\d+)".r

  val values = for (i <- input) yield i match {case r(sx,sy,bx,by) => (Pos(sx.toInt,sy.toInt), Pos(bx.toInt,by.toInt))}
  
  val sensors = values.map(_._1).toArray
  val beacons = values.map(_._2).toArray

  // https://www.appsloveworld.com/scala/100/10/how-to-functionally-merge-overlapping-number-ranges-from-a-list
  @annotation.tailrec final def collapse(rs: List[Range], sep: List[Range] = Nil): List[Range] = rs match {
    case x :: y :: rest =>
      if (y.start > x.end) collapse(y :: rest, x :: sep)
      else collapse((x.start to (x.end max y.end)) :: rest, sep)
    case _ =>
      (rs ::: sep).reverse
  }
  def merge(rs: List[Range]): List[Range] = collapse(rs.sortBy(_.start))

  def search (yTarget: Int, mn: Int, mx: Int): List[Range] = {
    def impossible(i: Int, y: Int): Range = {
      val s = sensors(i)
      val b = beacons(i)
      val hdist = (Math.abs(s.x - b.x) + Math.abs(s.y - b.y)) - Math.abs(s.y - y)
      val start = s.x - hdist
      val end   = s.x + hdist
      (if (start < mn) mn else start) to (if (end > mx) mx else end)
    }
    val z = for (i <- 0 until sensors.size)
      yield impossible(i, yTarget)

    merge(z.filter(!_.isEmpty).toList)
  }

  val yTarget = 2000000 // 10 for the example

  println("answer1=" + (search(yTarget, Int.MinValue, Int.MaxValue).head.size - beacons.toSet.count(_.y == yTarget)))

  val mn = 0
  val mx = 4000000 // 20 for the example
  val yc = (mn to mx).find(search(_, 0, mx).size > 1).get
  val xc = search(yc, 0, mx).tail(0).head - 1

  println("answer2=" + ((BigInt(xc) * BigInt(4000000)) + BigInt(yc))) // not mx!
}

object Day3 extends App {

  import scala.io.Source

  val f = Source.fromFile("../../../data/Day3.txt")

  val packs = f.getLines().toList.map(x => x.toList)

  println("answer1=" + 
      packs.map(x => {
        val a = x.splitAt(x.size/2)
        val b = a._1.toSet.intersect(a._2.toSet).head.toInt
        val c = if (b >= 'A' && b <= 'Z') b.toInt - 64 + 26 else b.toInt - 96
        c
      }).sum)

  val groups = f.reset().getLines().toList.map(x => x.toList).grouped(3).toList

  println("answer2=" + 
      groups.map(x => {
        val b = x(0).intersect(x(1).intersect(x(2))).head
        val c = if (b >= 'A' && b <= 'Z') b.toInt - 64 + 26 else b.toInt - 96
        c
      }).sum)
}

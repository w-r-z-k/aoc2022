object Day4 extends App {

  import scala.io.Source

  val f = Source.fromFile("../../../data/Day4.txt")

  val lines = f.getLines().toList

  val ranges = lines.map(x => x.split(Array('-',',')).map(_.toInt))

  println("answer1=" + ranges.map(a => {((a(0) <= a(2) && a(1) >=a (3))) || ((a(2) <= a(0) && a(3) >= a(1)))}).count(_ == true) )
  println("answer1=" + ranges.map(a => {(a(0) > a(3) || a(1) < a(2))}).count(_ == false) )

}

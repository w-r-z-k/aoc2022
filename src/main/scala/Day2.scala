object Day2 extends App { // Not my best work.  I was still trying to recall scala on Day 2

  import scala.io.Source

  val commands = Source.fromFile("../../../data/Day2.txt").getLines().toList

  val total = commands.map(x => {
    val first = x(0)
    val second = x(2)

    (if (first == 'A') {
       if (second == 'X') 3
       else if (second == 'Y') 6
       else 0
    } else if (first == 'B') {
       if (second == 'Y') 3
       else if (second == 'X') 0
       else 6
    } else if (first == 'C') {
       if (second == 'Z') 3
       else if (second == 'X') 6
       else 0
    } else 0) + (if (second == 'X') 1 else if (second == 'Y') 2 else 3)

  }).sum

  println ("answer1 = " + total)

  val total2 = commands.map(x => {
    val first = x(0)
    val second = x(2)
    
    if (first == 'A') {
       if (second == 'X') 0 + 3
       else if (second == 'Y') 3 + 1
       else 6 + 2
    } else if (first == 'B') {
       if (second == 'X') 0 + 1
       else if (second == 'Y') 3 + 2 
       else 6 + 3
    } else if (first == 'C') {
       if (second == 'X') 0 + 2
       else if (second == 'Y') 3 + 3
       else 6 + 1
    } else 0

  }).sum

  println ("answer2 = " + total2)

}

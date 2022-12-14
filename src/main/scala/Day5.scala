object Day5 extends App {

  import scala.io.Source
  import scala.collection.mutable.Stack

  val f = Source.fromFile("../../../data/Day5.txt")

  val lines = f.getLines().toList // all lines

  val crates = lines.takeWhile(_.length() > 1) // lines with crate info

  val index = crates.last.toList.zipWithIndex.filter(x => x._1 != ' ').unzip._2.toArray // char posns of crates

  val stacks = new Array[Stack[Char]](index.size)

  (0 until index.size).foreach(i => stacks.update(i, new Stack[Char](10)))

  crates.init.foreach(l => {
    (0 until index.size).foreach(i => {
      val c = l.charAt(index(i))
      if (c != ' ') stacks(i).push(c)
    })
  })

  (0 until index.size).foreach(i => stacks.update(i, stacks(i).reverse))

  val stacks2 = new Array[Stack[Char]](index.size)

  (0 until index.size).foreach(i => stacks2.update(i, stacks(i).clone))  // keep a copy for part 2

  val ops = lines.filter(l => l.startsWith("move"))

  val op = raw"move (\d+) from (\d+) to (\d+)".r

  ops.foreach(o => 
    o match {
       case op(count, from, to) => {
         val f = stacks(from.toInt - 1)
         val t = stacks(to.toInt - 1)
         (0 until count.toInt).foreach(i => t.push(f.pop()))
       }
    }
  )

  println("answer1=" + stacks.map(x => x.top).mkString)

  ops.foreach(o => 
    o match {
       case op(count, from, to) => {
         val f = stacks2(from.toInt - 1)
         val t = stacks2(to.toInt - 1)
         val h = new Stack[Char](count.toInt) // a transfer stack
         (0 until count.toInt).foreach(i => h.push(f.pop())) // pop n
         (0 until count.toInt).foreach(i => t.push(h.pop())) // push n
       }
    }
  )

  println("answer2=" + stacks2.map(x => x.top).mkString)
}

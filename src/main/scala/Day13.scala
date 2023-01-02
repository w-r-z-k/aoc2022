object Day13 extends App {

  import scala.io.Source

  val input = Source.fromFile("../../../data/Day13.txt").getLines().toList

  sealed trait Item {
    def value: Int
    def isEmpty: Boolean
    def head: Item
    def tail: Item
  }
  case class ListOfItems(l: List[Item]) extends Item {
    override def toString = "[" + l.mkString(",") + "]"
    def appended(i: Item) = ListOfItems(l.appended(i))
    def value = 0
    def isEmpty = l.isEmpty
    def head = l.head
    def tail = ListOfItems(l.tail)
  }
  case class Integer(i: Int) extends Item {
    override def toString = i.toString
    def value = i
    def isEmpty = false
    def head = Integer(i)
    def tail = Integer(i)
  }

  def readList(s: String): ListOfItems = {
     def readList0(tokens: List[String]): (List[String], ListOfItems) = {
       var l = ListOfItems(Nil)
       var ts = tokens
       while (!ts.isEmpty) {
            ts.head match {
              case "[" => val (nts, nl) = readList0(ts.tail) ; l = l.appended(nl) ; ts = nts
              case "," => ts = ts.tail
              case "]" => return (ts.tail, l)
              case num => ts = ts.tail ; l = l.appended(Integer(num.toInt))
            }
       }

       (Nil, l)
     }
     readList0(("[0-9]+|,|\\[|\\]".r findAllIn(s)).toList.tail)._2
  }

  def compare(left: String, right: String): Boolean = {

    def compare0(l: Item, r: Item): Int = {// <0 :== l<r, 0 :== l==r, >0 :== l>r
      if (l.isInstanceOf[Integer] && r.isInstanceOf[Integer]) {
        l.value - r.value
      }
      else if (l.isInstanceOf[Integer] && !r.isInstanceOf[Integer])
        compare0(ListOfItems(List(l)), r)
      else if (!l.isInstanceOf[Integer] && r.isInstanceOf[Integer])
        compare0(l, ListOfItems(List(r)))
      else {// both are lists
        if (l.isEmpty && !r.isEmpty) -1
        else if (!l.isEmpty && r.isEmpty) 1
        else if (l.isEmpty && r.isEmpty) 0
        else {
          val c = compare0(l.head, r.head)
          if (c == 0)
            compare0(l.tail, r.tail)
          else
            c
        }
      }
    }

    val c = compare0(readList(left), readList(right))
    c < 0
  }

  val z = for ((p, i) <- input.grouped(3).zipWithIndex
    if compare(p(0), p(1)))
  yield i

  println("answer1=" + z.toList.map(_ + 1).sum)

  val sorted = input.filter(l => l.length > 0).appended("[[2]]").appended("[[6]]").sortWith((a, b) => compare(a, b))

  println("answer2=" + ((sorted.indexOf("[[2]]") + 1) * (sorted.indexOf("[[6]]") + 1)))
}

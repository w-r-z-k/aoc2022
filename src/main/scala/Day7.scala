object Day7 extends App {

  import scala.io.Source

  val input = Source.fromFile("../../../data/Day7.txt").getLines().toList

  case class Dir(name: String, parent: Dir) {
    var size: Int = 0
    var list: List[Dir] = Nil
    def find(name: String): Dir = list.find{e => e.name == name}.get
    def add(e: Dir) = list = list :+ e
  }
      
  var cd: Dir = new Dir("/", null)

  val root = cd

  for (l <- input) { // parse command script and create dir tree
    if (l.startsWith("$ cd ")) {
      val c = l.drop(5)
      if (c == "..") cd = cd.parent
      else if (c != "/") cd = cd.find(c)
    } else if (l.startsWith("dir ")) {
      val name = l.drop(4)
      cd.add(new Dir(name, cd))
    } else if (l.startsWith("$ ls")) { // ignore "$ ls"
    } else {
      val size = l.takeWhile(c => c != ' ')
      cd.size = cd.size + size.toInt
    }
  }

  def update(dir: Dir): Int = {// update rolled-up dir counts
    if (dir.list != Nil)
      dir.size = dir.size + dir.list.foldLeft(0)((x, y) => x + update(y))
    dir.size
  }

  cd = root

  val used = update(root)

  def tally(dir: Dir, sum: Int): Int = {// strange sum of sizes <= 100K
    if (dir.list == Nil) {
      (if (dir.size  <= 100000) dir.size else 0)
    } else {
      val temp = (if (dir.size  <= 100000) dir.size else 0)
      temp + dir.list.foldLeft(0)((x, y) => x + tally(y, sum))
    }
  }

  println("answer1 = " + tally(root, 0))

  def best(dir: Dir, current: Int, required: Int): Int = {
    var b = (if (dir.size >= required && dir.size < current) dir.size else current)
    if (dir.list != Nil)
      b = dir.list.foldLeft(b)((x, y) => best(y, x, required))
    b
  }

  println("answer2 = " + best(root, used, 30000000 - (70000000 - used)))
}

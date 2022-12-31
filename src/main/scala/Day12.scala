object Day12 extends App {

  import scala.io.Source
  import scala.collection.mutable.Queue
  import scala.collection.mutable.HashMap

  val f = Source.fromFile("../../../data/Day12.txt")

  val input = f.getLines().toList

  val contour = (for (i <- input) yield i.toArray).toArray

  val xl = contour.size
  val yl = contour(0).size

  type Node = (Int, Int)

  def extractMarker(marker: Char, elevation: Char): Node = {
     val x = input.indexWhere(_.contains(marker))
     val y = input(x).indexWhere(_ == marker)
     contour(x)(y) = elevation
     (x, y)
  }

  val start = extractMarker('S', 'a')
  val end   = extractMarker('E', 'z')

  def findPaths(n: Node): List[Node] = {
    for(d <- List((-1,0),(1,0),(0,-1),(0,1))
        if (n._1 + d._1 >= 0 && n._1 + d._1 < xl)
        if (n._2 + d._2 >= 0 && n._2 + d._2 < yl)
        if (contour(n._1 + d._1)(n._2 + d._2) - contour(n._1)(n._2) <= 1))
      yield (n._1 + d._1, n._2 + d._2)
  }

  // bfs() adapted from: "Algorithms, 4th Edition" by Robert Sedgewick and Kevin Wayne.
  // Ref: https://algs4.cs.princeton.edu/41graph/BreadthFirstPaths.java.html

  def bfs(s: Node): Int = { // breadth-first search from a single source
      val marked = new HashMap[Node, Boolean]()  // marked[v] = is there an s-v path
      val edgeTo = new HashMap[Node, Node]()     // edgeTo[v] = previous edge on shortest s-v path
      val distTo = new HashMap[Node, Int]()      // distTo[v] = number of edges shortest s-v path
      val q: Queue[Node] = new Queue[Node]()
      val nNodes = xl * yl
      for (x <- 0 until xl; y <- 0 until yl) distTo((x, y)) = Int.MaxValue
      distTo(s) = 0
      marked(s) = true;
      q.enqueue(s);

      while (!q.isEmpty) {
          val v: Node = q.dequeue();
          for (w <- findPaths(v)) {
              if (!marked.getOrElse(w, false)) {
                  edgeTo(w) = v;
                  distTo(w) = distTo(v) + 1;
                  marked(w) = true;
                  q.enqueue(w);
              }
          }
      }
      distTo(end)
  }

  println("answer1=" + bfs(start))

  println("answer2=" + (for (x <- 0 until xl; y <- 0 until yl
                             if (contour(x)(y) == 'a'))
                          yield ((x, y), bfs((x, y)))).map(_._2).sorted.head)
}

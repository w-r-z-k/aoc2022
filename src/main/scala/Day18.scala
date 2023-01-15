import scala.collection.immutable.HashSet

object Day18 extends App { // Scala version 2.13.10

  val input = scala.io.Source.fromFile("../../../data/Day18.txt").getLines()
  
  case class Block(x: Int, y: Int, z: Int)

  val blocks: List[Block] = input.map{case l => {l.split(",").toList.map(_.toInt) match {
                                                                        case List(x, y, z, _*) => Block(x, y, z)
                                                                        case _ => Block(0,0,0)}}}.toList
  val shape = HashSet[Block](blocks: _*)

  val xm = shape.map(_.x).max // maximum x-coord in the shape
  val ym = shape.map(_.y).max // maximum y-coord in the shape
  val zm = shape.map(_.z).max // maximum z-coord in the shape, so the shape is contained in a mx by my by mz area

  def neighbours(b: Block): Set[Block] = {// if given block is within the cube area containing the shape plus the 
    if (b.x >= 1 || b.y >= 1 || b.z >= 1 || b.x <= xm || b.y <= ym || b.z <= zm) {// surrounding margin, return:
      Set.empty ++ (if (b.x - 1 >= 0     ) Set(Block(b.x - 1, b.y, b.z)) else Set.empty) // cube to the left and
                ++ (if (b.x + 1 <= xm + 1) Set(Block(b.x + 1, b.y, b.z)) else Set.empty) // cube to the right and
                ++ (if (b.y - 1 >= 0     ) Set(Block(b.x, b.y - 1, b.z)) else Set.empty) // cube above and 
                ++ (if (b.y + 1 <= ym + 1) Set(Block(b.x, b.y + 1, b.z)) else Set.empty) // cube below and
                ++ (if (b.z - 1 >= 0     ) Set(Block(b.x, b.y, b.z - 1)) else Set.empty) // cube one closer and
                ++ (if (b.z + 1 <= zm + 1) Set(Block(b.x, b.y, b.z + 1)) else Set.empty) // cube one away
    } else
      Set.empty // given block is not within the cube area containing the shape, so return an empty set
  }
  // The surface area of the shape is the open surfaces of all the blocks in the shape summed together.
  // The surface area of any given block in the shape is the surface area of a block with all sides
  // open (i.e. 6 :== 6 sides) less 1 for each surface that intersects with it's neighbouring blocks.
  def surface(shape: Set[Block]) = (for(b <- shape.toList) yield {6 - (shape intersect neighbours(b)).size}).sum

  val answer1 = surface(shape) // keep this result.  it will be needed in Part 2
  println(s"answer1=" + answer1)

  def dfs(start: Block): Vector[Block] = {// Tail recursive depth first search from any given block
    @annotation.tailrec                   // returning the blocks in the path searched
    def go(stack: List[Block], visited: Set[Block], path: Vector[Block]): Vector[Block] = stack match {
      case Nil => path                // termination condition is that there're no blocks left to search
      case head :: rest => {
        if (visited.contains(head)) { // if the head of the list has already been visited, there's
          go(rest, visited, path)     // nothing to do but to recurse with the rest blocks left to search
        } else {// If this is the 1st visit to the 1st block in the search list, get a list of it's neighbours
                // and exclude blocks in the shape itself and exclude any neighbour that's already been visited.
          val unvisited = neighbours(head).toList.filterNot(shape.contains).filterNot(visited.contains)
          go(unvisited ++ rest, visited + head, path :+ head) // add these blocks to the search list minus the
        }                                                     // current block (head), add the current block to
      }                                                       // the list of blocks visited, and add the current
    }                                                         // block to the accumulated path
    go(List(start), Set.empty, Vector.empty) // DFS from the start. starting with an empty visit list and path
  }

  val memo = scala.collection.mutable.HashMap[Block, Boolean]() // memoize results of trapped() for efficiency

  def trapped(b: Block) = { // Is the block trapped within a bubble?
    if (shape.contains(b))
      false                 // The given block is part of the shape, so by definition it is not trapped 
    else if (memo.isDefinedAt(b))
      memo(b)               // If we already got and recorded the answer, just return it
    else {
      val bs = dfs(b)       // Do a depth first search from this block
      // Did we hit the edge (i.e. any coord is < 1 or > max for the cude area)?
      val hit_an_edge = bs.map(_.x).min == 0 || bs.map(_.x).max == xm + 1 ||
                        bs.map(_.y).min == 0 || bs.map(_.y).max == ym + 1 ||
                        bs.map(_.z).min == 0 || bs.map(_.z).max == zm + 1

      memo.addAll(bs.map{b => b -> !hit_an_edge}) // All blocks in the path share the same answer

      !hit_an_edge // If we did not hit an edge, this block is trapped in a bubble
    }
  }

  def internal = {// the internal shape is the set of all blocks trapped in a bubble by the shape
    (for (x <- 1 to xm; y <- 1 to ym; z <- 1 to zm   // search all blocks in the cubic area
         if (!shape.contains(Block(x,y,z)))          // except the blocks part of the shape
         if (trapped(Block(x, y, z))))               // and yield the given block only if it's
      yield (Block(x, y, z))).toSet                  // trapped in a bubble
  }
  // Part 2's answer is the external shape area less the external area of the internal shape
  println(s"answer2=${answer1 - surface(internal)}")
}

object Day17 extends App { // Scala version 2.13.10

  val input = scala.io.Source.fromFile("../../../data/Day17.txt").getLines().toList.head.toList
  
  case class Rock (h: Int, w: Int, foot: Array[Int], top: Array[Int], left: Array[Int], right: Array[Int])
  //    0    1    2    3    4    Shape Index -- shapes given in the question
  //  0123 0123 0123 0123 0123
  // 0####0 #  0  # 0#   0##
  // 1    1### 1  # 1#   1##     Instead of coordinates I decided to use contours from the 3 sides based on 
  // 2    2 #  2### 2#   2       the upper left (-1,0) as the control point of the shape.  So, for example, 
  // 3    3    3    3#   3       the right side of shape 1 is 2 units right of the control point.           
  // -------------------------  
  //  1    3    3    4    2    height
  //  4    3    3    1    2    width       ________________
  //  0000 121  222  3    11   foot[]                      \
  //  0000 101  220  0    00   top[]                        \
  //  0    101  220  0000 00   left[]                        |
  //  3    121  222  0000 11   right[]                       V

  //                     h  w  foot            top             left            right
  val rocks = Array(Rock(1, 4, Array(0,0,0,0), Array(0,0,0,0), Array(0)      , Array(3)       ),
                    Rock(3, 3, Array(1,2,1)  , Array(1,0,1)  , Array(1,0,1)  , Array(1,2,1)   ),
                    Rock(3, 3, Array(2,2,2)  , Array(2,2,0)  , Array(2,2,0)  , Array(2,2,2)   ),
                    Rock(4, 1, Array(3)      , Array(0)      , Array(0,0,0,0), Array(0,0,0,0) ),
                    Rock(2, 2, Array(1,1)    , Array(0,0)    , Array(0,0)    , Array(1,1)     ))

  case class Pos (x: Int, y: Int)

  // For every rock type (r) and location in the gas input (c), store the first first rock index (n) seen
  // and accumulate a list of height increments (h) so that to use in detecting repeating patterns of (r,c) and h.
  var rc_nhs = scala.collection.mutable.Map[(Int,Int),(Int,List[Int])]() // (r,c) -> (n,List(h))
  var nh = Vector[Int]()    // I think this is easiest to understand as an iterative proceedure as opposed 
                            // to a functional definition.  But maybe it's just that I had trouble 
  def run(rounds: Int) = {  // conceiving it in functional terms.  Part 2 is functional, since it is based
                            // on pure calculations from the info gathered in the Part 1 process.
    val w = 7  // width of room - fixed
    var c = 0  // current index location in input of wind/gas directions
    var r = 0  // rock ID -- goes from 0 to 4 and recycles
    
    val blocks = new scala.collection.mutable.ListBuffer[Pos]() // will hold all the blocks in the chamber

    blocks.addAll(for (x <- 0 until w) yield Pos(x, 0)) // start with a flat floor defined at y = 0

    def highest = blocks.map(_.y).max // highest y-coord in all our blocks is our tower height

    // def printBlocks(tower: Int) = {
    //   for (y <- tower to 1 by -1) {
    //     val l = for (x <- 0 until w) yield (if (blocks.contains(Pos(x, y))) '#' else '.')
    //     println("|" + l.mkString + "|")
    //   }
    //   println("+-------+")
    // }

    def drop(r: Int, n: Int) = {
      val rs = rocks(r) // rock shape
      var p = Pos(2, highest + 3 + rs.h) // rock appears here, 3 units above tower

      def stopped: Boolean = {
        val pds = for (x <- 0 until rs.w) yield Pos(p.x + x, p.y - 1 - rs.foot(x)) 
        !pds.forall(!blocks.contains(_))
      }

      def canMove(gas: Int): Boolean = {
        if (p.x + gas + rs.w - 1 < w && p.x + gas >= 0) { // check walls
          // check rock obstructions
          if (gas == 1) {// moving right
            val prs = for (y <- 0 until rs.h) yield Pos(p.x + rs.right(y) + 1, p.y - y) 
            prs.forall(!blocks.contains(_))
          } else {// moving left
            val pls = for (y <- 0 until rs.h) yield Pos(p.x + rs.left(y) - 1, p.y - y) 
            pls.forall(!blocks.contains(_))
          }
        } else
          false
      }

      def recordBlocks: Unit = {// add stopped clock to the block list
        val bs = for (y <- 0 until rs.h; x <- rs.left(y) to rs.right(y)) yield Pos(p.x + x, p.y - y)
        blocks.addAll(bs.toList)
      }

      val h0 = highest // initial tower height at start of drop -- used to calculate the delta (i.e. h)
      val c0 = c  // initial input location at start of drop

      while ({
        val gas = if (input(c) == '<') -1 else 1  // left :== -1. right :== 1
        c = c + 1
        if (c % input.size == 0) c = 0 // recycle input
        if (canMove(gas)) p = Pos(p.x + gas, p.y) // move left or right is able to
        if (!stopped) {
          p = Pos(p.x, p.y - 1) // drop down one unit
          true // continue loop
        } else
          recordBlocks // put the blocks used by the just stopped rock in the block list
          false // end loop
      }) {}

      val h = highest - h0 // increment in tower height for this drop

      if (rc_nhs.isDefinedAt((r, c0))) {// Update the repeat map: (r,c) -> (n,List(h))
        val n = rc_nhs((r, c0))._1 // only 1st n value seen is kept. Need loc'n of n in 1st repeating segment
        rc_nhs((r, c0)) = (n, rc_nhs((r, c0))._2 :+ h)
      } else {
        rc_nhs((r, c0)) = (n, List(h))
      }

      h // return the increment in tower height for this drop
    }

    for (n <- 0 until rounds) {

      nh = nh :+ drop(r, n) // add the height of the drop to the height list

      def flat = {// this is an optimization - no need to keep blocks in the block list past a flat line
        val hs = for (i <- 0 until w) yield (blocks.filter(_.x == i).map(_.y).max)
        hs.count(_ == hs.head) == w // flat :== top of tower is the same across entire width of chamber?
      }

      if (n != 0 && flat) {
        val h = highest // save the height before clearing the block list or your floor will be at y = 0!
        blocks.clear()
        blocks.addAll(for (x <- 0 until w) yield Pos(x, h)) // draw a flat line at y = the current height
      }

      r = (r + 1) % 5 // next rock
    }
    highest // return the height of the tower
  }

  val answer1_target = 2022 // from the Part 1 question
  var t = System.nanoTime // solution is slow, especially Part 2, so let's time it (P1=2secs, P2=30sec)

  println(s"answer1=${run(answer1_target)}" + " (%1.3f seconds)".format((System.nanoTime - t) / 1e9d))

  if (answer1_target < input.size) { // For part 2, f the input length exceeds the number of rounds
    rc_nhs.clear()                     //   we won't get a full repeating segment on which to base the
    nh = Vector[Int]()               //   calculations for answer2 using 2022. So do another, longer, run.
    run(input.size * 2)              //   (Note: The test input doesn't require it.)
  }

  // rocks           height
  // ======          ======
  // prefix          prefix
  // + (repeat * n)  + (repeat * n)
  // + suffix        + suffix            <K> :== Known (i.e. total_rocks given in question 1T)
  // -----------     ------------        <?> :== Unknown                           
  // total_rocks     total_height        <C> :== Calculate                         
  //                                     <P> :== Plug-in from previous calculation or find
  // Determine:                          <F> :== Find
  // ==========
  //  total_rocks<K>  = rocks_prefix<F>  + (rocks_repeat<F>  * n_repeats<?>) + rocks_suffix<?>  // model
  //  total_rocks<K>  - rocks_prefixi<F> = (rocks_repeat<F>  * n_repeats<?>) + rocks_suffix<?>  // re-arrange
  // (total_rocks<K>  - rocks_prefix<P>) /  rocks_repeat<P>  = n_repeats<C>                     // therefore
  // (total_rocks<K>  - rocks_prefix<P>) %  rocks_repeat<P>  =                 rocks_suffix<C>  // therefore
  //  total_height<C> = height_prefix<F> + (height_repeat<F> * n_repeats<P>) + height_suffix<F> // & Q.E.D.

  // So I need to find: (the rest can be calulated from the above equations)
  // ==================
  // rocks_repeat  :== count(repeat list)
  // rocks_prefix  :== highest rock index in the repeating segment minus the size of that segment
  // height_prefix :== sum(heights in the height list 'nh' from start to rocks_prefix)
  // height_repeat :== sum(heights in the repeat list)
  // height_suffix :== sum(heights in the repeat list between start of repeat and rocks_suffix)

  val total_rocks = 1_000_000_000_000L // from the Part 2 question

  // Convert this map: (r,c) -> (n,List(h)) to this simplified map: (r,c) -> (height, count, n)
  val rl = rc_nhs.map{case (k, v) => { // v is (n,List(1,2,5,5,5,5,5,5,5,5))  i.e. list of heights
  /* run list */        val s = v._2.map(h => h -> v._2.count(_ == h)).toSet // Set((1,1),(2,1),(5,8))
                        val ss = s.toList.sortWith(_._2 > _._2)              // Set((5,8),(2,1),(1,1))
                        k -> (ss.head._1, ss.head._2, v._1) // (r,c) -> (height, count, n)
                     }}                                     // (k,v) ->   ._1     ._2 ._3

  val two_longest_runs = rl.values.map(_._2).toSet.toList.sortWith(_ > _).take(2)
  val rl2 = rl.values.filter(x => two_longest_runs.contains(x._2)) // rl with only 2 highest counts
  val height_repeat = rl2.map(_._1).sum // sum of heights in rl2
  val rocks_repeat = rl2.size // size of rl2 is the number of rocks in the repeating segment
  val rl2nmax = rl2.map(_._3).max // highest rock index in the repeating segment
  val rocks_prefix = rl2nmax - rocks_repeat + 1
  val height_prefix = nh.slice(0, rocks_prefix).sum
  val rocks_suffix = (total_rocks - rocks_prefix.toLong) % rocks_repeat.toLong
  val heights_in_repeat = nh.slice(rl2nmax - rocks_repeat + 1, rl2nmax)
  val height_suffix = heights_in_repeat.take(rocks_suffix.toInt).sum
  val n_repeats = (total_rocks - rocks_prefix) / rocks_repeat
  val total_height = height_prefix + (height_repeat * n_repeats) + height_suffix

  println("answer2=" + total_height + " (%1.3f seconds)".format((System.nanoTime - t) / 1e9d))
}

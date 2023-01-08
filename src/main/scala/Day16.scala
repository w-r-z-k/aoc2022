object Day16 extends App { // Scala version 2.13.10

  val input = scala.io.Source.fromFile("../../../data/Day16.txt").getLines().toList
  
  val r = "Valve ([A-Z]+) has flow rate=(\\d+); tunnels* leads* to valves* ([A-Z, ]+)".r

  final case class V(rate: Int, edge: List[String]) // Create a graph of valve names (include the pressure rate)

  val g = (for (i <- input) yield i match { case r(id,rate,edges) => id -> V(rate.toInt, edges.split(", ").toList) }).toMap

  // Use Floyd–Warshall algorithm to get shortest paths between each pair of nodes.  Other than this, the problem
  // is not really much of a graph problem.  (Ref: https://en.wikipedia.org/wiki/Floyd–Warshall_algorithm)
  val dist = new scala.collection.mutable.HashMap[String, Int]()       // let dist be a |V| × |V| array of minimum
  dist.addAll(for (i <- g.keys; j <- g.keys) yield (i + j -> 100000))  // distances initialized to ∞ (infinity)
                                                                       //   (note: using Int.MaxValue will overflow below)
  val es = g.map{case (k, v) => for (e <- v.edge) yield k + e}.flatten // get all edges from the graph
  
  for (e <- es) dist(e) = 1                          // for each edge (u, v) do                                    
                                                     //     dist[u][v] ← w(u, v)  // The weight of the edge (u, v) 
  for (i <- g.keys) dist(i + i) = 0                  // for each vertex v do                                       
                                                     //     dist[v][v] ← 0                                         
  for (k <- g.keys)                                  // for k from 1 to |V|                                  
    for (i <- g.keys)                                //     for i from 1 to |V|                              
      for (j <- g.keys)                              //         for j from 1 to |V|                          
        if (dist(i + j) > dist(i + k) + dist(k + j)) //             if dist[i][j] > dist[i][k] + dist[k][j]  
            dist(i + j) = dist(i + k) + dist(k + j)  //                 dist[i][j] ← dist[i][k] + dist[k][j] 
                                                     //             end if                                   

  def distance(from: String, to: String) = dist.getOrElse(from + to, Int.MaxValue - 100000)

  def rate(v: String) = g.getOrElse(v, V(0, Nil)).rate // Map of value names to pressure rates

  val closed = g.filter{case (k, v) => v.rate > 0}.keys.toList // Only non-0 rate valves matter

  // Setup now complete.  Due to difference in part1 vs. part2, I have two seperate simulations: maxflow & maxflow2

  def maxflow(closed: List[String], start: String): Int = {// Recursive DFS of all non-0 valve orderings

    def maxflow0(closed: List[String], start: String, time: Int, pressureInc: Int, total: Int): Int = {

      val total1 = total + (pressureInc * (if (time < 30) (30 - time) else 0)) // total pressure to end of time
                                                                               // available due to this valve increase
      if (closed.isEmpty)
        total1 // All valves open.  So above equation accounts for the total pressure.
      else (for (v <- closed) // Find the total pressure for each selection of next closed value to open
              yield (
                if (time >= 30)
                  total1 // Out of time.  Above equation is the final pressure achieved.
                else
                  maxflow0(closed.filter(_ != v), v, time + distance(start, v) + 1, rate(v), total1)
              )).max // select the highest pressure from all of the above optional paths taken
    }
    maxflow0(closed, start, 0, 0, 0);
  }
  
  println("answer1=" + maxflow(closed, "AA"))

  def maxflow2(closed: List[String], start: String, period: Int): Int = {// Recursive DFS of all non-0 valve orderings

    var maxSoFar = 0

    def maxflow0(closed: List[String], start1: String, time1: Int, pressureInc1: Int,
                                       start2: String, time2: Int, pressureInc2: Int, total: Int): Int = {

      val newTotal = total + (pressureInc1 * (if (time1 < period) (period - time1) else 0))
                           + (pressureInc2 * (if (time2 < period) (period - time2) else 0)) // total pressure to end
                                                                        // time available due to this valve increase
      val result =
        if (closed.isEmpty || (time1 >= period && time2 >= period))
          newTotal // All valves open or both out of time.  So above equation accounts for the total pressure.
        else {
          def upperbound = {
            def minTime(v: String) = if (start2 == "") time1 + distance(start1, v)
                                     else (time1 + distance(start1, v)).min(time2 + distance(start2, v))
            // Path's final total pressure cannot be higher than going to & openning all closed valves simultaneously
            newTotal + (for (v <- closed) yield ((period - minTime(v)) * rate(v))).filter(_ > 0).sum
          }

          if (maxSoFar > upperbound) // without this optimization, the run time is astronomical
            newTotal
          else // Find the total pressure for each selection of next closed value to open
            (for (v1 <- closed; v2 <- closed if (v1 != v2 && v1!=""))
               yield (
                   maxflow0(closed.filter(v => v != v1 && v != v2), 
                     v1, time1 + (if (time1 >= period) 0 else distance(start1, v1) + 1), rate(v1),
                     v2, time2 + (if (time2 >= period) 0 else distance(start2, v2) + 1), rate(v2), newTotal)
               )).max // select the highest pressure from the above optional paths taken
        }

      if (result > maxSoFar) maxSoFar = result // keep track of highest pressure seen so far to reduce paths

      result
    }     // if the list of closed values is odd, add a blank one so that we can go exactly two-by-two
    maxflow0(if (closed.size % 2 == 1) closed :+ "" else closed, start, 0, 0, start, 0, 0, 0);
  }

  val t1 = System.nanoTime
  println("answer2=" + maxflow2(closed, "AA", 30 - 4) + " (%1.3f seconds)".format((System.nanoTime - t1) / 1e9d))
}

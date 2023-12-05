import ox.scl._

class ConcGraphSearch[N](g: Graph[N]) extends GraphSearch[N](g){
  /**The number of workers. */
  val numWorkers = 8

  /** Perform a depth-first search in g, starting from start, for a node that
    * satisfies isTarget. */
  def apply(start: N, isTarget: N => Boolean): Option[N] = {
    val stack = new ConcStack[(N, List[N])](numWorkers)
    val pathFound = new SyncChan[N]
    stack.push((start, List(start)))
    // A single worker
    def worker = thread("worker"){
      repeat{
        val (n, path) = stack.pop
        for(n1 <- g.succs(n)){
          if(isTarget(n1)) pathFound!(n1) // done!
          else stack.push((n1, path :+ n1))
        }
      }
      pathFound.close // causes coordinator to close down
    }

    // Variable that ends up holding the result; written by coordinator. 
    var result: Option[N] = None

    def coordinator = thread("coordinator"){
      attempt{ result = Some(pathFound?()) }{result = None}
      stack.shutdown // close stack; this will cause most workers to terminate
      pathFound.close // in case another thread has found solution
    }

    val workers = || (for(_ <- 0 until numWorkers) yield worker)
    run(workers || coordinator)
    result
  }
}

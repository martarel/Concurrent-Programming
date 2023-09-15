import ox.scl._

/** Simulation of right-handed Dining Philosophers.*/
object Variant1{
  val N = 5 // Number of philosophers

  // Simulate basic actions
  def Eat = Thread.sleep(500)
  def Think = Thread.sleep(scala.util.Random.nextInt(900))
  def Pause = Thread.sleep(500)

  type Command = Boolean
  val Pick = true; val Drop = false
 
  /** A single philosopher. */
  def phil(me: Int, right: ![Command], left: ![Command]) = thread("Phil"+me){
    repeat{
      Think
      println(s"$me sits"); Pause
      right!Pick; println(s"$me picks up right fork"); Pause
      left!Pick; println(s"$me picks up left fork"); Pause
      println(s"$me eats"); Eat
      right!Drop; Pause; left!Drop; Pause; 
      println(s"$me leaves")
    }
  } 

  /** A single fork. */
  def fork(me: Int, right: `?`[Command], left: `?`[Command]) = thread("Fork"+me){
    serve(
      right =?=> {
        x => assert(x == Pick); val y = right?(); assert(y == Drop)
      }
      |
      left =?=> {
        x => assert(x == Pick); val y = left?(); assert(y == Drop)
      }
    )
  } 

  /** The complete system. */ 
  def system = {
    // Channels to pick up and drop the forks:
    val philToRightFork, philToLeftFork = Array.fill(N)(new SyncChan[Command])
    // philToRightFork(i) is from Phil(i) to Fork(i);
    // philToLeftFork(i) is from Phil(i) to Fork((i-1)%N)
    val allPhils = || ( 
      for (i <- 0 until N)
      yield phil(i, philToRightFork(i), philToLeftFork(i))
    )
    val allForks = || ( 
      for (i <- 0 until N)
      yield fork(i, philToRightFork(i), philToLeftFork((i-1+N)%N))
    )
    allPhils || allForks
  }

  /** Run the system. */
  def main(args : Array[String]) = { run(system) }
}

  
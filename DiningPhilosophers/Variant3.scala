import ox.scl._

/** Simulation of the Dining Philosophers with timeouts to avoid deadlock. */
object Variant3{
  val N = 5 // Number of philosophers

  // Simulate basic actions
  def Eat = Thread.sleep(500)
  def Think = Thread.sleep(scala.util.Random.nextInt(900))
  def Pause = Thread.sleep(500)
  def Pause(n: Int) = Thread.sleep(n);

  type Command = Boolean
  val Pick = true; val Drop = false
 
  /** A single philosopher. */
  def phil(me: Int, left: ![Command], right: ![Command]) = thread("Phil"+me){
    repeat{
      Think
      println(s"$me sits"); Pause
      var rightAcquired = false
      left!Pick; println(s"$me picks up left fork"); Pause
      rightAcquired = right.sendWithin(me*100)(Pick)
      repeat(!rightAcquired){
        left!Drop;
        println(s"$me dropping left fork to wait for right fork");
        left!Pick; println(s"$me picks up left fork again"); Pause
        //Set waiting time to 100ms * philosopher number to avoid deadlock
        rightAcquired = right.sendWithin(me*100)(Pick)
        Pause(me*100)
      }
      println(s"$me picks up right fork"); Pause
      println(s"$me eats"); Eat
      left!Drop; Pause; right!Drop; Pause
      println(s"$me leaves")
    }
  } 

  /** A single fork. */
  def fork(me: Int, left: `?`[Command], right: `?`[Command]) = thread("Fork"+me){
    serve(
      left =?=> {
        x => assert(x == Pick); val y = left?(); assert(y == Drop)
      }
      |
      right =?=> {
        x => assert(x == Pick); val y = right?(); assert(y == Drop)
      }
    )
  } 

  /** The complete system. */ 
  def system = {
    // Channels to pick up and drop the forks:
    val philToLeftFork, philToRightFork = Array.fill(N)(new SyncChan[Command])
    // philToLeftFork(i) is from Phil(i) to Fork(i);
    // philToRightFork(i) is from Phil(i) to Fork((i-1)%N)
    val allPhils = || ( 
      for (i <- 0 until N)
      yield phil(i, philToLeftFork(i), philToRightFork(i))
    )
    val allForks = || ( 
      for (i <- 0 until N)
      yield fork(i, philToRightFork((i+1)%N), philToLeftFork(i))
    )
    allPhils || allForks
  }

  /** Run the system. */
  def main(args : Array[String]) = { run(system) }
}

  
import ox.scl._

/** Simulation of the Dining Philosophers with a Butler.*/
object Variant2{
  val N = 5 // Number of philosophers

  // Simulate basic actions
  def Eat = Thread.sleep(500)
  def Think = Thread.sleep(scala.util.Random.nextInt(900))
  def Pause = Thread.sleep(500)

  type Command = Boolean
  val Pick = true; val Drop = false

  type ButlerCommunication = String
  val Ask = "ask"; val Granted = "granted"; val Leave = "leave"

  def butler(askIn: `?`[ButlerCommunication], askOut: ![ButlerCommunication], tellIn:`?`[ButlerCommunication]) = thread("Butler"){
    var seated = 0;
    serve(
        (seated < N - 1) && askIn =?=> {
          x => ;println(s"Permission granted for ${x.takeRight(1)}"); seated = seated + 1; askOut!Granted
        }
        |
        tellIn =?=> {
          x => assert(x == Leave); seated = seated - 1;
        }
    )
  }
 
  /** A single philosopher. */
  def phil(me: Int, askIn: `?`[ButlerCommunication], askOut: ![ButlerCommunication], tellOut: ![ButlerCommunication], left: ![Command], right: ![Command]) = thread("Phil"+me){
    repeat{
      Think
      println(s"$me asking for permission");
      var allowed = false
      askOut!(Ask + me.toString)
      allowed = askIn ? () == Granted
      println(s"$me sits"); Pause
      left!Pick; println(s"$me picks up left fork"); Pause
      right!Pick; println(s"$me picks up right fork"); Pause
      println(s"$me eats"); Eat
      left!Drop; Pause; right!Drop; Pause
      println(s"$me leaves")
      tellOut!Leave
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
    val ask, grant, tell, recieve = new SyncChan[ButlerCommunication]
    val allPhils = || ( 
      for (i <- 0 until N)
      yield phil(i, grant, ask, tell, philToLeftFork(i), philToRightFork(i))
    )
    val allForks = || ( 
      for (i <- 0 until N)
      yield fork(i, philToRightFork((i+1)%N), philToLeftFork(i))
    )
    allPhils || allForks || butler(ask, grant, recieve)
  }

  /** Run the system. */
  def main(args : Array[String]) = { run(system) }
}

  
import ox.scl._

/** Simulation of the Dining Philosophers with a Butler to avoid deadlock.*/
/** Butler essentially implements the RAServer trait from lectures.*/
class Butler(numSeats: Int){
  type Message = String
  type Philosopher = Int
  private type ReplyChan = Chan[Message]
  /* Channel for requesting a seat. */
  private val requestChan = new BuffChan[ReplyChan](5)
  /* Channel for returning a seat. */
  private val returnChan = new SyncChan[Message]

  private def server = thread{
    // Reply channels for requests that cannot be served immediately.
    val pending = new scala.collection.mutable.Queue[ReplyChan]
    // Invariant: if pending is non-empty, then all seats are taken.
    var takenSeats = 0
    serve(
      requestChan =?=> { replyChan => 
        println(s"Hanndling a new philosopher, currently $takenSeats out of $numSeats seats taken");
        if(takenSeats == numSeats) pending.enqueue(replyChan) // Philospher has to wait
        else{  // Allow philosopher to sit
            takenSeats += 1; replyChan!"Sit!"
        }
        }
      | returnChan =?=> { message =>  
          println(s"Handling a return, currently $takenSeats out of $numSeats seats taken");
          if(pending.nonEmpty) pending.dequeue()!"Sit!" // Allocate seat to blocked philosopher
          else takenSeats -= 1
      }
    )
  }
  // Fork off the server
  server.fork

  /** Request a seat. */
  def requestSeat(me:Philosopher): Message = {
    val replyChan =  new SyncChan[Message]
    requestChan!replyChan  // send request
    val response = replyChan?() // wait for response
    println(s"$response $me")
    response
  }

  /** Return a seat. */
  def returnSeat(me: Philosopher) = {
    println(s"Hope you enjoyed the meal $me")
    returnChan!"Returning seat"
  }
}

object Variant2{
  val N = 5 // Number of philosophers

  // Simulate basic actions
  def Eat = Thread.sleep(500)
  def Think = Thread.sleep(scala.util.Random.nextInt(900))
  def Pause = Thread.sleep(500)

  type Command = Boolean
  val Pick = true; val Drop = false
 
  /** A single philosopher. */
  def phil(me: Int, left: ![Command], right: ![Command], butler: Butler) = thread("Phil"+me){
    repeat{
      Think
      println(s"$me asking for permission");
      butler.requestSeat(me);
      println(s"$me sits"); Pause
      left!Pick; println(s"$me picks up left fork"); Pause
      right!Pick; println(s"$me picks up right fork"); Pause
      println(s"$me eats"); Eat
      left!Drop; Pause; right!Drop; Pause
      println(s"$me leaves"); butler.returnSeat(me)
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
    // philToLeftFork(i) is from Phil(i) to Fork(i);
    // philToRightFork(i) is from Phil(i) to Fork((i-1)%N)
    val philToLeftFork, philToRightFork = Array.fill(N)(new SyncChan[Command])
    val butler = new Butler(N-1)
    val allPhils = || ( 
      for (i <- 0 until N)
      yield phil(i, philToLeftFork(i), philToRightFork(i), butler)
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

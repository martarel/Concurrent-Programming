import ox.scl._

/** The trait for a Sleeping Tutor protocol. */
trait SleepingTutor{
  /** A tutor waits for students to arrive. */
  def tutorWait : Unit

  /** A student arrives and waits for the tutorial. */
  def arrive : Unit
  
  /** A student receives a tutorial. */
  def receiveTute : Unit

  /** A tutor ends the tutorial. */
  def endTeach : Unit
}

// =======================================================

/** A Sleeping Tutor protocol implemented using semaphores. */

class SleepingTutorSemaphores extends SleepingTutor{
    private var numStudentsWaiting = 0
    private val tutorWaitingSem = new SignallingSemaphore
    private val studentWaitingSem = new SignallingSemaphore
    private val mutex = new MutexSemaphore

    def tutorWait = {
      tutorWaitingSem.down
    }
    def arrive = {
      mutex.down
      numStudentsWaiting += 1
      if (numStudentsWaiting == 2){
        tutorWaitingSem.up
      }
      mutex.up
    }

    def receiveTute = {
      studentWaitingSem.down
      numStudentsWaiting -= 1
      if (numStudentsWaiting == 1){
        studentWaitingSem.up
      }
    }

    def endTeach = {
      studentWaitingSem.up
    }
}

// =======================================================

import scala.util.Random

object SleepingTutorSimulation{
  private val st: SleepingTutor = new SleepingTutorSemaphores

  def student(me: String) = thread("Student"+me){
    while(true){
      Thread.sleep(Random.nextInt(2000))
      println("Student "+me+" arrives")
      st.arrive
      println("Student "+me+" ready for tutorial")
      st.receiveTute
      println("Student "+me+" leaves")
    }
  }

  def tutor = thread("Tutor"){
    while(true){
      println("Tutor waiting for students")
      st.tutorWait
      println("Tutor starts to teach")
      Thread.sleep(Random.nextInt(2000))
      println("Tutor ends tutorial")
      st.endTeach
      Thread.sleep(Random.nextInt(2000))
    }
  }

  def system = tutor || student("Alice") || student("Bob")

  def main(args: Array[String]) = {
    run(system)
  }
}

// =======================================================

import scala.concurrent.duration._

object SleepingTutorTest{
  private val duration = 5
  private val st: SleepingTutor = new SleepingTutorSemaphores
  private val log = new Log[String](3)

  def student(me: String, id : Int) = thread("Student"+me){
    val deadline = duration.seconds.fromNow
    while(deadline.hasTimeLeft){
      Thread.sleep(Random.nextInt(2000))
      log.add(id, "Student "+me+" arrives")
      st.arrive
      log.add(id, "Student "+me+" ready for tutorial")
      st.receiveTute
      log.add(id, "Student "+me+" leaves")
    }
  }

  def tutor = thread("Tutor"){
    val deadline = duration.seconds.fromNow
    while(deadline.hasTimeLeft){
      log.add(0, "Tutor waiting for students")
      st.tutorWait
      log.add(0, "Tutor starts to teach")
      Thread.sleep(Random.nextInt(2000))
      log.add(0, "Tutor ends tutorial")
      st.endTeach
      Thread.sleep(Random.nextInt(2000))
    }
  }

  def system = tutor || student("Alice", 1) || student("Bob", 2)

  def checkLog = {
    val logs = log.get
    var studentsCanLeave = false
    var aliceWaiting = false
    var bobWaiting = false
    for (entry <- logs){
      entry match {
        case "Tutor starts to teach" => assert(aliceWaiting && bobWaiting); aliceWaiting = false; bobWaiting = false;
        case "Tutor ends tutorial" => studentsCanLeave = true
        case s if s.endsWith("arrives") => if (s.split(" ")(1) == "Alice") aliceWaiting = true else bobWaiting = true; studentsCanLeave = false
        case s if s.endsWith("leaves") => assert(studentsCanLeave);
        case _ => {}
      }
    }
  }

  def main(args: Array[String]) = {
    run(system)
    checkLog
  }
}
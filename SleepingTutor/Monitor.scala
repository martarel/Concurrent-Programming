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

/** A Sleeping Tutor protocol implemented using monitors. */

class SleepingTutorMonitor extends SleepingTutor{
    //guaranteed false from any call after first student leaving arrive block, before first entering of endTeach block
    //guranteed true after leaving endTeach block, before both students have entered arrive block
    private var tutorialDone = false
    //guaranteed true from any call after first entering of tutorWait, before first entering of endTeach block
    //guaranteed false after exiting endTeach until first entering of tutorWait
    private var tutorWaiting = false
    private var numStudents = 0

    //tutor won't leave this block until both students have called arrive
    def tutorWait = synchronized{
        tutorWaiting = true
        while (numStudents != 2) {
          notifyAll()
          wait()
        }
    }

    //student won't leave this block until tutor has called tutorWait
    def arrive = synchronized{
        numStudents += 1
        while (!tutorWaiting){
          notifyAll()
          wait()
        }
        notifyAll()
        tutorialDone = false
    }

    //student won't leave this block until tutor has called tutorDone
    def receiveTute = synchronized{
        notifyAll()
        while (!tutorialDone){
          notifyAll()
          wait()
        }
    }

    def endTeach = synchronized{
        tutorialDone = true
        tutorWaiting = false
        numStudents = 0
        notifyAll()
    }
}

// =======================================================

import scala.util.Random

object SleepingTutorSimulation{
  private val st: SleepingTutor = new SleepingTutorMonitor

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
  private val duration = 3
  private val st: SleepingTutor = new SleepingTutorMonitor
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
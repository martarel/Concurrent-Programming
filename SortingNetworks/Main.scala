import ox.scl._
import scala.util.Random

//Task 1
/** A single comparator, inputting on in0 and in1, and outputting on out0
* (smaller value) and out1 (larger value). */
def comparator(in0: `?`[Int], in1: `?`[Int], out0: ![Int ], out1: ![Int ]): ThreadGroup = thread{
    repeat(!in0.isClosed && !in1.isClosed){
        val x = in0 ? ()
        val y = in1 ? ()
        if (x < y) {
            out0 ! x
            out1 ! y
        } else {
            out0 ! y
            out1 ! x
        }
    }
}

//Task 2
def singleComparator(in0: `?`[Int], in1: `?`[Int], out0: ![Int ], out1: ![Int ]): ThreadGroup = thread{
    val x = in0 ? ()
    val y = in1 ? ()
    if (x < y) {
        out0 ! x
        out1 ! y
    } else {
        out0 ! y
        out1 ! x
    }
}

/** A sorting network for four values. */
def sort4(ins: List [`?`[Int]], outs: List [![ Int ]]): ThreadGroup = thread{
    require(ins.length == 4 && outs.length == 4)
    val (in0 :: in1 :: in2 :: in3 :: Nil) = ins
    val (out0 :: out1 :: out2 :: out3 :: Nil) = outs
    val t0, t1, t2, t3, t4, t5 = new SyncChan[Int]
    run(singleComparator(in0, in2, t0, t2) || singleComparator(in1, in3, t1, t3) || singleComparator(t0, t1, out0, t4) || singleComparator(t2, t3, t5, out3) || singleComparator(t4, t5, out1, out2))
}

def testSort4: Unit = {
    for (i <- 1 to 100) {
        print(".")
        val xs = Array.fill(4)(scala.util.Random.nextInt(100))
        val ys = new Array[Int](4)
        val ins = List.fill(4)(new SyncChan[Int])
        val outs = List.fill(4)(new SyncChan[Int])
        def sender = thread{ for(i <- 0 until 4) {ins(i) ! xs(i)}}
        def receiver = thread{for(i <- 0 until 4) {ys(i) = outs(i) ? ()}}
        run(sender || sort4(ins, outs) || receiver)
        assert(xs.sorted.sameElements(ys))
    }
    println()
    println("Done :)")
}

//Task 3
/** Insert a value input on in into a sorted sequence input on ins.
* Pre: ins.length = n && outs.length = n+1, for some n >= 1.
* If the values xs input on ins are sorted, and x is input on in, then a
* sorted permutation of x::xs is output on ys. */
def insert(ins: List [`?`[Int]], in: `?`[Int], outs: List [![Int]]): ThreadGroup = thread{
    val n = ins.length; require(n >= 1 && outs.length == n+1)
    val x = in ? ()
    val y = ins(0) ? ()
    if (n == 1) {
        if (x <= y) {
            outs(0) ! x
            outs(1) ! y
        } else {
            outs(0) ! y
            outs(1) ! x
        }
    }
    else if (x <= y) {
        outs(0) ! x
        outs(1) ! y
        for (i <- 2 until (n+1)) {
            val z = ins(i-1) ? ()
            outs(i) ! z
        }
    } else {
        outs(0) ! y 
        val in2 = new SyncChan[Int]
        run(insert(ins.tail, in2, outs.tail) || thread{in2 ! x})
    }
}

def testInsert: Unit = {
    val size = 100
    for (i <- 1 to 100) {
        print(".")
        val xs = List.fill(size)(scala.util.Random.nextInt(100)).sorted
        val x = scala.util.Random.nextInt(100)
        val ys = new Array[Int](size+1)
        val ins = List.fill(size)(new SyncChan[Int])
        val outs = List.fill(size+1)(new SyncChan[Int])
        val in = new SyncChan[Int]
        def sender = thread{in ! x; for(i <- 0 until size) {ins(i) ! xs(i)}}
        def receiver = thread{for(i <- 0 until (size+1)) {ys(i) = outs(i) ? ()}}
        run(sender || insert(ins, in, outs) || receiver)
        assert((x :: xs).sorted.sameElements(ys))
    }
    println()
    println("Done :)")
}

//Task 4
def fastInsert(ins: List [`?`[Int]], in: `?`[Int], outs: List [![Int]]): ThreadGroup = thread{
    val n = ins.length; require(n >= 1 && outs.length == n+1)
    val x = in ? ()
    val xs = new Array[Int](n)
    for (i <- 0 until n) {
        xs(i) = ins(i) ? ()
    }
    val ys = new Array[Int](n+1)
    var l = 0
    var r = n
    //Invariant: 0 <= l < r <= n; x should be inserted in xs[l..r); xs[0..l) <= x < xs[r..n); xs[0..l) and xs[r..n) have been sent to correct outs
    while (l < r-1) {
        println(l, r)
        val m = (l+r)/2 //l < m < r
        val y = xs(m)
        if (x < y) {
            ys(m+1) = y
            for (i <- (m+1) until r) {
                ys(i+1) = xs(i)
            }
            r = m
        } else {
            ys(m) = y
            for (i <- l until m) {
                ys(i) = xs(i)
            }
            l = m+1
        }
    }
    xs(l) = x
    for (i <- 0 until n+1) {
        outs(i) ! xs(i)
    }
}

def testFastInsert: Unit = {
    val size = 10
    for (i <- 1 to 1) {
        print(".")
        val xs = List.fill(size)(scala.util.Random.nextInt(100)).sorted
        println(xs)
        val x = scala.util.Random.nextInt(100)
        println(x)
        val ys = new Array[Int](size+1)
        val ins = List.fill(size)(new SyncChan[Int])
        val outs = List.fill(size+1)(new SyncChan[Int])
        val in = new SyncChan[Int]
        def sender = thread{in ! x; for(i <- 0 until size) {ins(i) ! xs(i)}}
        def receiver = thread{for(i <- 0 until (size+1)) {ys(i) = outs(i) ? ()}}
        run(sender || fastInsert(ins, in, outs) || receiver)
        assert((x :: xs).sorted.sameElements(ys))
    }
    println()
    println("Done :)")
}

//Task 5
/** Insertion sort. */
def insertionSort(ins: List [`?`[Int]], outs: List [![Int]]) : ThreadGroup = thread{
    val n = ins.length; require(n >= 2 && outs.length == n)
    val xs = new Array[Int](n)
    val x1 = ins(0) ? ()
    xs(0) = x1
    //Invariant: xs [0::i) is sorted and we aim to make ins(i):xs[0..i) sorted and set it to xs[0..i]
    for (i <- 1 until n) {
        val ys = new Array[Int](i+1)
        val insUtil = List.fill(i)(new SyncChan[Int])
        val inUtil = new SyncChan[Int]
        val outsUtil = List.fill(i+1)(new SyncChan[Int])
        def sender = thread{val next = ins(i) ? (); inUtil ! next; for(i <- 0 until i) {insUtil(i) ! xs(i)}}
        def receiver = thread{for(i <- 0 until (i+1)) {ys(i) = outsUtil(i) ? ()}}
        run(sender || insert(insUtil, inUtil, outsUtil) || receiver)
        for (i <- 0 until (i+1)) {
            xs(i) = ys(i)
        }
    }
    def sender = thread{for(i <- 0 until n) {outs(i) ! xs(i)}}
    run(sender)
}


def testInsertionSort: Unit = {
    val size = 40
    for (i <- 1 to 49) {
        print(".")
        val xs = Array.fill(size)(scala.util.Random.nextInt(100))
        val ys = new Array[Int](size)
        val ins = List.fill(size)(new SyncChan[Int])
        val outs = List.fill(size)(new SyncChan[Int])
        def sender = thread{for(i <- 0 until size) {ins(i) ! xs(i)}}
        def receiver = thread{for(i <- 0 until size) {ys(i) = outs(i) ? ()}}
        run(sender || insertionSort(ins, outs) || receiver)
        assert(xs.sorted.sameElements(ys))
    }
    println()
    println("Done :)")
}

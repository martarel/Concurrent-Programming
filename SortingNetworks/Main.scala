object Main {
    /* A single comparator, inputting on in0 and in1, and outputting on out0 (smaller value) and out1 (larger value). */
    def comparator(in0: ?[Int], in1: ?[Int], out0: ![Int ], out1: ![Int ]): ThreadGroup = thread("Comparator") {
        attempt {
            val x = in0 ? ()
            val y = in1 ? ()
            if (x < y) {
                out0 ! x
                out1 ! y
            } else {
                out0 ! y
                out1 ! x
            }
        } {
            out0.endOfStream
            out1.endOfStream
        }
    }
    
}

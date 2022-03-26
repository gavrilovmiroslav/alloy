
package object Util {
    def time[R](block: => R): (R, Long) = {
        val t0 = System.nanoTime()
        val result = block
        val t1 = System.nanoTime()
        (result, (t1 - t0))
    }

    def clamp(v: Float, down: Float, up: Float): Float = {
        var vn = v
        if (vn > up) vn = up 
        if (vn < down) vn = down  
        vn
    }
}
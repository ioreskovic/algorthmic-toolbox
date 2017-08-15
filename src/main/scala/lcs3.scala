import scala.io.StdIn

object lcs3 {
  def solve[T](aSeq: Array[T], bSeq: Array[T], cSeq: Array[T]): Int = {
    val dp = Array.tabulate(aSeq.length + 1, bSeq.length + 1, cSeq.length + 1)( (_, _, _) => 0)

    for (i <- 1 to aSeq.length) {
      for (j <- 1 to bSeq.length) {
        for (k <- 1 to cSeq.length) {
          if (aSeq(i - 1) == bSeq(j - 1) && aSeq(i - 1) == cSeq(k - 1)) {
            dp(i)(j)(k) = dp(i - 1)(j - 1)(k - 1) + 1
          } else {
            val opts = List(
              dp(i)(j)(k - 1),
              dp(i)(j - 1)(k),
              dp(i - 1)(j)(k)
            )

            dp(i)(j)(k) = opts.max
          }
        }
      }
    }

    dp.last.last.last
  }

  def main(args: Array[String]): Unit = {
    val aLen = StdIn.readLine().toInt
    val a = StdIn.readLine().split(" ").map(_.toInt)

    val bLen = StdIn.readLine().toInt
    val b = StdIn.readLine().split(" ").map(_.toInt)

    val cLen = StdIn.readLine().toInt
    val c = StdIn.readLine().split(" ").map(_.toInt)

    val lcs = solve(a, b, c)

    println(lcs)
  }
}

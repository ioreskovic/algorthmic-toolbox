import scala.io.StdIn

object edit_distance {
  case class EditDistance(s1: String, s2: String) {
    private val m = Array.tabulate(s1.length + 1, s2.length + 1){
      case (0, j) => j
      case(i, 0) => i
      case _ => 0
    }

    def step(idx1: Int, idx2: Int): EditDistance = {
      val c1 = s1(idx1)
      val c2 = s2(idx2)

      if (c1 == c2) {
        m(idx1 + 1)(idx2 + 1) = m(idx1)(idx2)
      } else {
        val opt1 = m(idx1)(idx2 + 1)
        val opt2 = m(idx1)(idx2)
        val opt3 = m(idx1 + 1)(idx2)

        m(idx1 + 1)(idx2 + 1) = math.min(opt1, math.min(opt2, opt3)) + 1
      }

      this
    }

    def solution: Int = {
      m.last.last
    }
  }

  def solve(s1: String, s2: String): Int = {
    val editDistance = EditDistance(s1, s2)
    for (idx1 <- s1.indices; idx2 <- s2.indices) {
      editDistance.step(idx1, idx2)
    }
    editDistance.solution
  }

  def main(args: Array[String]): Unit = {
    val s1 = StdIn.readLine()
    val s2 = StdIn.readLine()
    println(solve(s1, s2))
  }
}

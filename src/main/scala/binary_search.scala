import scala.annotation.tailrec
import scala.io.StdIn

object binary_search {
  def main(args: Array[String]): Unit = {
    val arrF = search(StdIn.readLine().split(" ").map(_.toInt).tail) _
    val ress = StdIn.readLine().split(" ").map(_.toInt).tail.map(x => arrF(x))
    println(ress.mkString(" "))
  }

  def search[T](arr: Array[T])(x: T)(implicit ord: T => Ordered[T]): Int = {
    @tailrec
    def loop(from: Int, until: Int): Int = {
      if (until < from) -1
      else {
        val mid = (from + until) / 2
        val y = arr(mid)
        if (x == y) mid
        else if (x < y) loop(from, mid - 1)
        else loop(mid + 1, until)
      }
    }

    loop(0, arr.length - 1)
  }
}

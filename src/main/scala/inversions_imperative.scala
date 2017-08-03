import scala.io.StdIn

object inversions_imperative {
  case class ArrayInv(array: Array[Int], inversions: Long) {
    def length: Int = array.length
    def withInversions(nInversions: Long): ArrayInv = ArrayInv(array, nInversions)
    def halves: (ArrayInv, ArrayInv) = {
      val (l, r) = array.splitAt(array.length / 2)
      (ArrayInv(l, 0L), ArrayInv(r, 0L))
    }
    def apply(n: Int): Int = array(n)
  }

  def main(args: Array[String]): Unit = {
    StdIn.readLine()
    val input = StdIn.readLine().split(" ").map(_.toInt)
    val resultM = sort(ArrayInv(input, 0L))
    println(resultM.inversions)
  }

  def sort(arrInv: ArrayInv): ArrayInv = {
    if (arrInv.length < 2) arrInv.withInversions(0L)
    else {
      val (l, r) = arrInv.halves
      val (ls, rs) = (sort(l), sort(r))
      merge(ls, rs)
    }
  }

  def merge(aInv: ArrayInv, bInv: ArrayInv): ArrayInv = {
    val cArr = new Array[Int](aInv.length + bInv.length)
    var i = 0
    var j = 0
    var k = 0
    var inversions = 0L

    while (i < aInv.length && j < bInv.length) {
      if (aInv(i) <= bInv(j)) {
        cArr(k) = aInv(i)
        i = i + 1
      } else {
        cArr(k) = bInv(j)
        j = j + 1
        inversions = inversions + (aInv.length - i)
      }
      k = k + 1
    }

    while (i < aInv.length) {
      cArr(k) = aInv(i)
      i = i + 1
      k = k + 1
    }

    while (j < bInv.length) {
      cArr(k) = bInv(j)
      j = j + 1
      k = k + 1
    }

    ArrayInv(cArr, aInv.inversions + bInv.inversions + inversions)
  }
}

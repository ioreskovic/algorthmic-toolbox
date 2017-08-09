import scala.annotation.tailrec
import scala.io.StdIn

object primitive_calculator {
  abstract class Operation(f: Int => Int) {
    def apply(x: Int): Int = f(x)
    def inverse(y: Int): Option[Int]
  }

  case class Initial() extends Operation(x => x) {
    override def inverse(y: Int): Option[Int] = Some(y)
  }

  case class AddOne() extends Operation(x => x + 1) {
    override def inverse(y: Int): Option[Int] = {
      val x = y - 1
      if (x < 1) None
      else Some(x)
    }
  }

  case class MulTwo() extends Operation(x => x * 2) {
    override def inverse(y: Int): Option[Int] = {
      if (y < 2 || y % 2 != 0) None
      else Some(y / 2)
    }
  }

  case class MulThree() extends Operation(x => x * 3) {
    override def inverse(y: Int): Option[Int] = {
      if (y < 3 || y % 3 != 0) None
      else Some(y / 3)
    }
  }

  class State(n: Int) {
    private val numWays = Array.fill(n)(0)
    private val ops: Array[Operation] = Array.fill(n)(Initial())

    def ways(i: Int): Int = numWays(i - 1)

    def op(i: Int): Operation = ops(i - 1)

    def forNumber(i: Int, x: Int, o: Operation): State = {
      numWays(i - 1) = x
      ops(i - 1) = o
      this
    }

    override def toString: String = {
      (0 until n).map(i => "[" + (i + 1) + " => (" + numWays(i) + ", " + ops(i) + ")]").mkString("\n")
    }
  }

  type Operations = List[Operation]

  case class Solution(n: Int, steps: Int, inters: List[Int])

  def solve(n: Int): Solution = {
    val init = Initial()
    val add1 = AddOne()
    val mul2 = MulTwo()
    val mul3 = MulThree()

    val ops = List(mul3, mul2, add1)

    @tailrec
    def loop(i: Int, state: State): State = {
      if (i > n) state
      else if (i == 1) loop(i + 1, state)
      else {
        val (inverse, operation, ways) = ops.map { op => (op.inverse(i), op) }.filter { case (y, op) => y.isDefined }.map { case (y, op) => (y.get, op, state.ways(y.get) + 1) }.minBy(_._3)
        loop(i + 1, state.forNumber(i, ways, operation))
      }
    }

    @tailrec
    def reconstruct(i: Int, state: State, operations: Operations = Nil): Operations = {
      if (i == 1) operations
      else {
        val op = state.op(i)
        reconstruct(op.inverse(i).get, state, op :: operations)
      }
    }

    val state = loop(1, new State(n))
    val ways = state.ways(n)
    val operations = reconstruct(n, state)
    val inters = operations.scanLeft(1){ case (z, op) => op(z) }
    Solution(n, ways, inters)
  }

  def main(args: Array[String]): Unit = {
    val solution = solve(StdIn.readInt())
    println(solution.steps)
    println(solution.inters.mkString(" "))
  }
}

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

  class TheState(n: Int) {
    private val numWays = new Array[Int](n)
    private val parents = new Array[Option[Int]](n)

    def ways(i: Int): Int = numWays(i - 1)
    def parent(i: Int): Option[Int] = parents(i - 1)

    def record(i: Int, ways: Int, parent: Int): TheState = {
      numWays(i - 1) = ways
      parents(i - 1) = Some(parent)
      this
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

  lazy val add1 = AddOne()
  lazy val mul2 = MulTwo()
  lazy val mul3 = MulThree()

  @tailrec
  def reconstruct(maybeI: Option[Int], state: TheState, inters: List[Int]): List[Int] = maybeI match {
    case None => inters
    case Some(i) if i == 1 => reconstruct(None, state, 1 :: inters)
    case Some(i) => reconstruct(state.parent(i), state, i :: inters)
  }

  def reconstructImperative(n: Int, state: State): List[Int] = {
    var y = n
    var solution: List[Int] = Nil

    while (y != 1) {
      solution = y :: solution
      val op = state.op(y)
      y = op.inverse(y).get
    }

    1 :: solution
  }

  def solveImperative(n: Int): Solution = {
    val add1 = AddOne()
    val mul2 = MulTwo()
    val mul3 = MulThree()

    val state = new State(n)

    for (i <- 2 to n) {
      val maybe1 = add1.inverse(i)
      val maybe2 = mul2.inverse(i)
      val maybe3 = mul3.inverse(i)

      val ways1 = if (maybe1.isDefined) state.ways(maybe1.get) else Integer.MAX_VALUE
      val ways2 = if (maybe2.isDefined) state.ways(maybe2.get) else Integer.MAX_VALUE
      val ways3 = if (maybe3.isDefined) state.ways(maybe3.get) else Integer.MAX_VALUE

      if (ways1 <= ways2 && ways1 <= ways3) {
        state.forNumber(i, ways1 + 1, add1)
      } else if (ways2 <= ways3) {
        state.forNumber(i, ways2 + 1, mul2)
      } else {
        state.forNumber(i, ways3 + 1, mul3)
      }
    }

    val ways = state.ways(n)
    val inters = reconstructImperative(n, state)
    Solution(n, ways, inters)
  }


  def bo3(state: TheState, y: Int, operations: Operations): (Int, Int, Operation) = {
    var bestWays = Integer.MAX_VALUE
    var bestOp: Operation = Initial()
    var bestX = 0

    for (op <- operations) {
      val maybeX = op.inverse(y)
      if (maybeX.isDefined) {
        val x = maybeX.get
        val xWays = state.ways(x)
        if (xWays < bestWays) {
          bestWays = xWays
          bestOp = op
          bestX = x
        }
      }
    }

    (bestX, bestWays + 1, bestOp)
  }

  def bo3Fixed(state: TheState, y: Int): (Int, Int) = {
    val maybe1 = add1.inverse(y)
    val maybe2 = mul2.inverse(y)
    val maybe3 = mul3.inverse(y)

    val ways1 = if (maybe1.isDefined) state.ways(maybe1.get) else Integer.MAX_VALUE
    val ways2 = if (maybe2.isDefined) state.ways(maybe2.get) else Integer.MAX_VALUE
    val ways3 = if (maybe3.isDefined) state.ways(maybe3.get) else Integer.MAX_VALUE

    if (ways1 <= ways2 && ways1 <= ways3) {
      (maybe1.get, ways1 + 1)
    } else if (ways2 <= ways3) {
      (maybe2.get, ways2 + 1)
    } else {
      (maybe3.get, ways3 + 1)
    }
  }

  def solve(n: Int): Solution = {
    val add1 = AddOne()
    val mul2 = MulTwo()
    val mul3 = MulThree()

    val ops = List(mul3, mul2, add1)

    @tailrec
    def loop(i: Int, state: TheState): TheState = {
      if (i > n) state
      else if (i == 1) loop(i + 1, state)
      else {
//        val (inverse, ways) = ops.map { op => op.inverse(i) }.filter { x => x.isDefined }.map { maybeX => (maybeX.get, state.ways(maybeX.get) + 1) }.minBy(_._2)
//        val (inverse, ways, op) = bo3(state, i, ops)
        val (inverse, ways) = bo3Fixed(state, i)
        loop(i + 1, state.record(i, ways, inverse))
      }
    }

    val state = loop(1, new TheState(n))
    val ways = state.ways(n)
//    val inters = reconstruct(Some(n), state, Nil)
    val inters = List.fill(ways)(0)
    Solution(n, ways, inters)
  }

  def main(args: Array[String]): Unit = {
    val n = StdIn.readInt()

    val startD = System.currentTimeMillis()
    val solutionD = solve(n)
    val endD = System.currentTimeMillis()
    println("Declarative time: " + (endD - startD))
//    println("Declarative steps: " + solutionD.steps)
//    println("Declarative inters: " + solutionD.inters.mkString(" "))
        println(solutionD.steps)
        println(solutionD.inters.mkString(" "))

//    val startI = System.currentTimeMillis()
//    val solutionI = solveImperative(n)
//    val endI = System.currentTimeMillis()
//    println("Imperative time: " + (endI - startI))
//    println("Imperative steps: " + solutionI.steps)
//    println("Imperative inters: " + solutionI.inters.mkString(" "))
//    println(solutionI.steps)
//    println(solutionI.inters.mkString(" "))
  }
}

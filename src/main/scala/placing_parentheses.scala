import scala.annotation.tailrec
import scala.io.StdIn

object placing_parentheses {
  trait Expression

  case class Operator(f: (Long, Long) => Long) extends Expression {
    def apply(a: Long, b: Long): Long = f(a, b)
  }

  class Plus extends Operator(_ + _)
  class Minus extends Operator(_ - _)
  class Times extends Operator(_ * _)

  case class Number(value: Int) extends Expression

  case class Input(arr: Array[Expression]) {
    val n: Int = (arr.length + 1) / 2
    override lazy val toString: String = arr.mkString(" ")

    def nthOperator(n: Int): Operator = {
      arr(n * 2 + 1) match {
        case o @ Operator(_) => o
      }
    }

    def nthNumber(n: Int): Number = {
      arr(n * 2) match {
        case n @ Number(_) => n
      }
    }
  }

  case class MinState(input: Input) {
    private val n = (input.arr.length + 1) / 2

    private val min = Array.tabulate(n, n){
      case (i, j) if i == j => input.nthNumber(i).value.toLong
      case _ => 0L
    }

    def apply(x: Int, y: Int): Long = min(x)(y)

    def update(x: Int, y: Int, value: Long): MinState = {
      min(x)(y) = value
      this
    }
  }

  case class MaxState(input: Input) {
    private val n = (input.arr.length + 1) / 2

    private val max = Array.tabulate(n, n){
      case (i, j) if i == j => input.nthNumber(i).value.toLong
      case _ => 0L
    }

    def apply(x: Int, y: Int): Long = max(x)(y)

    def update(x: Int, y: Int, value: Long): MaxState = {
      max(x)(y) = value
      this
    }
  }

  case class State(input: Input) {
    val min: MinState = MinState(input)
    val max: MaxState = MaxState(input)
  }

  def parse(s: String): Input = {
    @tailrec
    def loop(cx: List[Char], res: List[Expression]): List[Expression] = cx match {
      case Nil => res
      case c :: cs => c match {
        case '+' => loop(cs, new Plus() :: res)
        case '-' => loop(cs, new Minus() :: res)
        case '*' => loop(cs, new Times() :: res)
        case _ => loop(cs, Number(c.toInt - '0') :: res)
      }
    }

    Input(loop(s.toList, Nil).reverse.toArray)
  }

  def solve(input: Input): Long = {
    val state = State(input)
    val n = input.n

    for (s <- 1 until n; i <- 0 until (n - s)) {
      val j = i + s

      var min = Long.MaxValue
      var max = Long.MinValue

      for (k <- i until j) {
        val operator = input.nthOperator(k)
        val maxMax = operator(state.max(i, k), state.max(k + 1, j))
        val maxMin = operator(state.max(i, k), state.min(k + 1, j))
        val minMax = operator(state.min(i, k), state.max(k + 1, j))
        val minMin = operator(state.min(i, k), state.min(k + 1, j))

        val options = List(maxMax, maxMin, minMax, minMin)

        min = (min :: options).min
        max = (max :: options).max
      }

      state.min(i, j) = min
      state.max(i, j) = max
    }

    state.max(0, n - 1)
  }

  def main(args: Array[String]): Unit = {
    val input = parse(StdIn.readLine())
    val solution = solve(input)
    println(solution)
  }
}
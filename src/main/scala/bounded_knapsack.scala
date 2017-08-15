import scala.io.StdIn

object bounded_knapsack {
  case class GoldBar(size: Int)

  case class Knapsack(capacity: Int, items: Int) {
    private val sol = Array.fill(capacity + 1, items + 1)(0)

    def capacityItemsValue(c: Int, i: Int): Int = {
      sol(c)(i + 1)
    }

    def updateCapacityItemsValue(c: Int, i: Int, v: Int): Knapsack = {
      sol(c)(i + 1) = v
      this
    }

    def maxValue: Int = {
      sol(capacity)(items)
    }
  }

  def solve(knapsack: Knapsack, items: Array[GoldBar]): Int = {

    def step(c: Int, i: Int): Unit = {
      val item = items(i)
      if (item.size > c) {
        knapsack.updateCapacityItemsValue(c, i, knapsack.capacityItemsValue(c, i - 1))
      } else {
        knapsack.updateCapacityItemsValue(c, i, math.max(knapsack.capacityItemsValue(c, i - 1), knapsack.capacityItemsValue(c - item.size, i - 1) + item.size))
      }
    }

    for (c <- 1 to knapsack.capacity; i <- items.indices) {
      step(c, i)
    }

    knapsack.maxValue
  }

  def main(args: Array[String]): Unit = {
    val capacity = StdIn.readLine().split(" ")(0).toInt
    val items = StdIn.readLine().split(" ").map(s => GoldBar(s.toInt))
    val knapsack = Knapsack(capacity, items.length)
    val maxValue = solve(knapsack, items)
    println(maxValue)
  }
}

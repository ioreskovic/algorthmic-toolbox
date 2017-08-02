package io.ioreskovic.coursera.w4

import scala.io.StdIn

object MajorityElement {
  def main(args: Array[String]): Unit = {
    StdIn.readLine()
    val input = StdIn.readLine().split(" ").map(_.toInt)
    val result = find(input)
    result match {
      case Some(_) => println(1)
      case None => println(0)
    }
  }

  def find[T](array: Array[T]): Option[T] = {
    def occurrences(from: Int, to: Int, z: T): Int = {
      array.view(from, to + 1).count(_ == z)
    }

    def loop(from: Int, to: Int): Option[T] = {
      if (from > to) None
      else if (from == to) Some(array(from))
      else {
        val mid = (from + to) / 2
        val maybeLeft = loop(from, mid)
        val maybeRight = loop(mid + 1, to)

        (maybeLeft, maybeRight) match {
          case (Some(x), _) if occurrences(from, to, x) > (to - from + 1) / 2 => Some(x)
          case (_, Some(x)) if occurrences(from, to, x) > (to - from + 1) / 2 => Some(x)
          case _ => None
        }
      }
    }

    loop(0, array.length - 1)
  }
}

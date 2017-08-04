package io.ioreskovic.coursera.w4


import scala.annotation.tailrec
import scala.io.StdIn

object PointsAndSegments {
  trait Point {
    def value: Int
    def priority: Int
  }

  case class QueryPoint(value: Int) extends Point {
    override def priority: Int = 2
  }

  case class SegmentStart(value: Int) extends Point {
    override def priority: Int = 0
  }

  case class SegmentEnd(value: Int) extends Point {
    override def priority: Int = 1
  }

  implicit val pointOrdering: Ordering[Point] = new Ordering[Point] {
    override def compare(x: Point, y: Point): Int = {
      val cmp = x.value.compareTo(y.value)
      if (cmp != 0) cmp
      else x.priority.compareTo(y.priority)
    }
  }

  type Segment = (SegmentStart, SegmentEnd)

  def parseSegment(s: String): Segment = {
    val info = s.split(" ")
    (SegmentStart(info(0).toInt), SegmentEnd(info(1).toInt))
  }

  def parseQueryPoints(s: String): List[QueryPoint] = {
    s.split(" ").map(e => QueryPoint(e.toInt)).toList
  }

  def main(args: Array[String]): Unit = {
    val metaInfo = StdIn.readLine().split(" ")
    val nSegments = metaInfo(0).toInt
    val nPoints = metaInfo(1).toInt
  }
}

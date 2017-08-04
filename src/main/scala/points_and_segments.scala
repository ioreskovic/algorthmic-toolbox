import scala.annotation.tailrec
import scala.io.StdIn

object points_and_segments {
  case class Point(value: Int, index: Int) {
    def isBefore(s: Segment): Boolean = {
      value < s.from
    }

    def isAfter(s: Segment): Boolean = {
      value > s.to
    }

    def isInside(s: Segment): Boolean = {
      value >= s.from && value <= s.to
    }
  }

  case class Segment(from: Int, to: Int)

  case class PointResult(p: Point, result: Int = 0) {
    def incrementBy(value: Int): PointResult = PointResult(p, result + value)
  }

  type Points = List[Point]
  type Segments = List[Segment]

  implicit val segmentOrdering: Ordering[Segment] = new Ordering[Segment] {
    def compare(x: Segment, y: Segment): Int = {
      val cmpFrom = x.from.compareTo(y.from)
      if (cmpFrom == 0) cmpFrom
      else x.to.compareTo(y.to)
    }
  }

  implicit val pointOrdering: Ordering[Point] = new Ordering[Point] {
    def compare(x: Point, y: Point): Int = {
      x.value.compare(y.value)
    }
  }

  implicit def parseSegment(s: String): Segment = {
    val info = s.split(" ")
    Segment(info(0).toInt, info(1).toInt)
  }

  implicit def parsePoint(s: String, index: Int): Point = {
    Point(s.toInt, index)
  }

  def scan(points: Points, segments: Segments): Seq[PointResult] = {
    @tailrec
    def loop(px: Points, sx: Segments, res: List[PointResult]): List[PointResult] = (px, sx, res) match {
      case (p :: ps, s :: ss, Nil) if p.isInside(s) => loop(px, sx, PointResult(p) :: res)
      case (p :: ps, s :: ss, Nil) if p.isBefore(s) => loop(ps, sx, PointResult(p) :: res)
      case (p :: ps, s :: ss, Nil) if p.isAfter(s) => loop(px, ss, PointResult(p) :: res)

      case (p :: ps, s :: ss, r :: rs) if r.p == p && p.isInside(s) => loop(px, ss, r.incrementBy(1) :: rs)
      case (p :: ps, s :: ss, r :: rs) if r.p == p && p.isBefore(s) => loop(ps, sx, res)
      case (p :: ps, s :: ss, r :: rs) if r.p == p && p.isAfter(s) => loop(px, ss, res)

      case (p :: ps, s :: ss, r :: rs) if r.p != p && p.isInside(s) => loop(px, sx, PointResult(p) :: res)
      case (p :: ps, s :: ss, r :: rs) if r.p != p && p.isBefore(s) => loop(ps, sx, PointResult(p) :: res)
      case (p :: ps, s :: ss, r :: rs) if r.p != p && p.isAfter(s) => loop(px, ss, PointResult(p) :: res)

      case (p :: ps, Nil, Nil) => loop(ps, sx, PointResult(p) :: res)
      case (p :: ps, Nil, r :: rs) if r.p == p => loop(ps, sx, res)
      case (p :: ps, Nil, r :: rs) => loop(ps, sx, PointResult(p) :: res)
      case _ => res
    }

    val sortedPoints = points.sorted(pointOrdering)
    loop(sortedPoints, segments.sorted(segmentOrdering), Nil).reverse
  }

  def main(args: Array[String]): Unit = {
    val metaInfo = StdIn.readLine().split(" ")
    val nSegments = metaInfo(0).toInt
    val nPoints = metaInfo(1).toInt

    val segments = (0 until nSegments).map(_ => parseSegment(StdIn.readLine())).toList
    val points = StdIn.readLine().split(" ").zipWithIndex.map(p => parsePoint(p._1, p._2)).toList

    val result = scan(points, segments).sortBy(_.p.index).map(_.result)
    println(result.mkString(" "))
  }
}

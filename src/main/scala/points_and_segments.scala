import scala.annotation.tailrec
import scala.io.StdIn

object points_and_segments {
  trait Point {
    def value: Int
    def priority: Int
  }

  case class QueryPoint(value: Int, originalIndex: Int) extends Point {
    override def priority: Int = 1
  }

  case class SegmentStart(value: Int) extends Point {
    override def priority: Int = 0
  }

  case class SegmentEnd(value: Int) extends Point {
    override def priority: Int = 2
  }

  case class Result(inclusions: Int, originalIndex: Int)

  implicit val pointOrdering: Ordering[Point] = new Ordering[Point] {
    override def compare(x: Point, y: Point): Int = {
      val cmp = x.value.compareTo(y.value)
      if (cmp != 0) cmp
      else x.priority.compareTo(y.priority)
    }
  }

  def parseSegment(s: String): List[Point] = {
    val info = s.split(" ")
    List(SegmentStart(info(0).toInt), SegmentEnd(info(1).toInt))
  }

  def parseQueryPoints(s: String): List[Point] = {
    s.split(" ").zipWithIndex.map{ case (e, i) => QueryPoint(e.toInt, i) }.toList
  }

  def main(args: Array[String]): Unit = {
    val metaInfo = StdIn.readLine().split(" ")
    val nSegments = metaInfo(0).toInt
    val nPoints = metaInfo(1).toInt

    val segments = (0 until nSegments).flatMap(_ => parseSegment(StdIn.readLine())).toList
    val queryPoints = parseQueryPoints(StdIn.readLine())

    val points = (segments ::: queryPoints).sorted(pointOrdering)

    val result = solve(points, Nil, 0, 0).sortBy(_.originalIndex)
    val outputs = result.map(_.inclusions)

    println(outputs.mkString(" "))
  }

  @tailrec
  def solve(px: List[Point], res: List[Result], starts: Int, ends: Int): List[Result] = px match {
    case SegmentStart(_) :: ps => solve(ps, res, starts + 1, ends)
    case SegmentEnd(_) :: ps => solve(ps, res, starts, ends + 1)
    case QueryPoint(_, idx) :: ps => solve(ps, Result(starts - ends, idx) :: res, starts, ends)
    case _ => res
  }
}

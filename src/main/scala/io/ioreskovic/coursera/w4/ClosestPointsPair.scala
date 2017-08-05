package io.ioreskovic.coursera.w4

import scala.annotation.tailrec
import scala.io.StdIn

object ClosestPointsPair {
  case class Point(x: Int, y: Int)
  type Distance = (Point, Point) => Double
  type Points = List[Point]

  def xOrdering: Ordering[Point] = new Ordering[Point] {
    override def compare(a: Point, b: Point): Int = a.x.compareTo(b.x)
  }

  def yOrdering: Ordering[Point] = new Ordering[Point] {
    override def compare(a: Point, b: Point): Int = a.y.compareTo(b.y)
  }

  def euclideanDistance: Distance = (a, b) => {
    math.sqrt(
      math.pow(a.x - b.x, 2) +
      math.pow(a.y - b.y, 2)
    )
  }

  def splitAtMedian(points: Points): (Points, Points, Point) = {
    val (before, after) = points.splitAt(points.length / 2)
    (before, after, after.head)
  }

  def buildStrip(points: Points, midPoint: Point, d: Double): Points = {
    @tailrec
    def loop(px: Points, res: Points): Points = px match {
      case (p :: ps) if math.abs(p.x - midPoint.x) < d => loop(ps, p :: res)
      case (p :: ps) => loop(ps, res)
      case _ => res.reverse
    }

    loop(points, Nil)
  }

  def forgo[T](list: List[T]): List[(T, T)] = {
    @tailrec
    def loop(ix: List[T], jx: List[T], res: List[(T, T)]): List[(T, T)] = (ix, jx) match {
      case (i :: Nil, _) => res
      case (i :: is, Nil) => loop(ix, is, res)
      case (i :: is, j :: Nil) => loop(is, Nil, (i, j) :: res)
      case (i :: is, j :: js) => loop(ix, js, (i, j) :: res)
      case _ => res
    }

    loop(list, Nil, Nil).reverse
  }

  def stripMin(strip: Points, halfWidth: Double): Double = {
    @tailrec
    def loop(ix: Points, jx: Points, min: Double): Double = (ix, jx) match {
      case (i :: Nil, _) => min
      case (i :: is, Nil) => loop(ix, is, min)
      case (i :: is, j :: Nil) if (j.y - i.y) < min => loop(is, Nil, math.min(min, euclideanDistance(i, j)))
      case (i :: is, j :: Nil) => loop(is, Nil, min)
      case (i :: is, j :: js) if (j.y - i.y) < min => loop(ix, js, math.min(min, euclideanDistance(i, j)))
      case (i :: is, j :: js) => loop(is, Nil, min)
      case _ => min
    }

    loop(strip, Nil, halfWidth)
  }

  def bruteForce(points: Points): Double = {
    @tailrec
    def loop(ix: Points, jx: Points, min: Double): Double = (ix, jx) match {
      case (i :: Nil, _) => min
      case (i :: is, Nil) => loop(ix, is, min)
      case (i :: is, j :: Nil) if euclideanDistance(i, j) < min => loop(is, Nil, euclideanDistance(i, j))
      case (i :: is, j :: Nil) => loop(is, Nil, min)
      case (i :: is, j :: js) if euclideanDistance(i, j) < min => loop(ix, js, euclideanDistance(i, j))
      case (i :: is, j :: js) => loop(ix, js, min)
      case _ => min
    }

    loop(points, Nil, Double.PositiveInfinity)
  }

  def closestDistance(points: Points): Double = {
    def loop(points: Points): Double = {
      if (points.length < 4) bruteForce(points)
      else {
        val (l, r, mp) = splitAtMedian(points)
        val (dl, dr) = (loop(l), loop(r))
        val d = math.min(dl, dr)
        math.min(d, stripMin(buildStrip(points, mp, d).sorted(yOrdering), d))
      }
    }

    loop(points.sorted(xOrdering))
  }

  def parsePoint(str: String): Point = {
    val info = str.split(" ")
    Point(info(0).toInt, info(1).toInt)
  }

  def main(args: Array[String]): Unit = {
    val nPoints = StdIn.readLine().toInt
    val points = (0 until nPoints).map(_ => parsePoint(StdIn.readLine())).toList
    val minDistance = closestDistance(points)
    println(minDistance)
  }
}

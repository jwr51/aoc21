package advent

import scala.io.Source
import scala.math.max

object Day05:

  def main(args: Array[String]): Unit =
    val input =
      Source
        .fromResource("day05.txt")
        .getLines()
        .map(_.split("""[^\d]+""").map(_.toInt))
        .map { case Array(x1, y1, x2, y2) => Line(Point(x1, y1), Point(x2, y2)) }
        .toSeq

    println(p1(input))
    println(p2(input))

  case class Point(x: Int, y: Int)
  case class Line(a: Point, b: Point) {
    val diagonal: Boolean = a.x != b.x && a.y != b.y
    val length: Int = max((b.x - a.x).abs, (b.y - a.y).abs)

    def points: Seq[Point] =
      val dx = (b.x - a.x).sign
      val dy = (b.y - a.y).sign
      (0 to length).map(n => Point(a.x + dx * n, a.y + dy * n))
  }

  def overlaps(lines: Seq[Line]): Int =
    lines.flatMap(_.points)
         .groupMapReduce(identity)(_ => 1)(_ + _)
         .count(_._2 > 1)

  def p1(input: Seq[Line]): Int = overlaps(input.filterNot(_.diagonal))

  def p2(input: Seq[Line]): Int = overlaps(input)

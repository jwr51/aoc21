package advent

import scala.math.max

object Day5 {

  def main(args: Array[String]): Unit = {
    val input = Advent.input(5)
    println(p1(input))
    println(p2(input))
  }

  case class Point(x: Int, y: Int)
  type Line = (Point, Point)

  private val Pattern = """(\d+),(\d+) -> (\d+),(\d+)""".r

  def parse(input: String): List[Line] =
    input.linesIterator
         .toList
         .map { case Pattern(x1, y1, x2, y2) => (Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt)) }

  def vents(diag: Boolean)(l: Line): Seq[Point] = {
    val (p, q) = l
    val dx = (q.x - p.x).sign
    val dy = (q.x - p.x).sign

    if (dx != 0 && dy != 0 && !diag) return List.empty

    val n = max((q.x - p.x).abs, (q.y - p.y).abs)
    (0 to n).map(c => Point(p.x + dx * c, p.y + dy * c))
  }

  def overlaps(fn: Line => Seq[Point])(lines: List[Line]): Set[Point] = {
    lines
      .flatMap(fn)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .filter(_._2 >= 2)
      .keySet
  }

  def p1(input: String): Any = {
    val lines = parse(input)
    overlaps(vents(diag = false))(lines).size
  }

  def p2(input: String): Any = {
    val lines = parse(input)
    overlaps(vents(diag = true))(lines).size
  }
}

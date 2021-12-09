package advent

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day9 {

  def main(args: Array[String]): Unit = {
    val input = Advent.input(9)
    val grid = input
      .linesIterator
      .toList
      .zipWithIndex
      .flatMap((row, y) => row.zipWithIndex.map((c, x) => Point(x, y) -> c.asDigit))
      .toMap

    println(p1(grid))
    println(p2(grid))
  }

  case class Point(x: Int, y: Int)

  def adjacent(p: Point): Seq[Point] = Seq(
    p.copy(x = p.x - 1),
    p.copy(x = p.x + 1),
    p.copy(y = p.y - 1),
    p.copy(y = p.y + 1)
  )

  def sinks(grid: Map[Point, Int]): Seq[Point] =
    grid.keys
        .toSeq
        .filter(p => grid(p) < adjacent(p).map(q => grid.getOrElse(q, 10)).min)

  def bfs(fn: Point => Seq[Point])(p: Point): LazyList[Point] = {
    def _bfs(q: Queue[Point], s: Set[Point]): LazyList[Point] = q match {
      case p +: ps => p #:: _bfs(ps ++ fn(p).filter(!s.contains(_)), s + p)
      case _       => LazyList.empty
    }

    p #:: _bfs(Queue.empty ++ fn(p), Set.empty)
  }

  def basin(grid: Map[Point, Int])(sink: Point): Set[Point] = {
    val fn: Point => Seq[Point] = p => adjacent(p).filter(grid.contains).filter(q => grid(q) < 9)
    bfs(fn)(sink).toSet
  }

  def p1(grid: Map[Point, Int]): Any =
    sinks(grid)
      .map(grid)
      .map(_ + 1)
      .sum

  def p2(grid: Map[Point, Int]): Any =
    sinks(grid)
      .map(basin(grid))
      .map(_.size)
      .sorted
      .takeRight(3)
      .product
}

package advent

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day09 {

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day09.txt").mkString
    val grid = input
      .linesIterator
      .toList
      .zipWithIndex
      .flatMap((row, y) => row.zipWithIndex.map((c, x) => Point(x, y) -> c.asDigit))
      .toMap

    println(p1(grid))
    println(p2(grid))
  }

  type Grid = Map[Point, Int]

  case class Point(x: Int, y: Int) {
    def neighbours(grid: Grid): Seq[Point] = {
      val neighbours = for
        nx <- x - 1 to x + 1
        ny <- y - 1 to y + 1
        if nx == x || ny == y
      yield Point(nx, ny)

      neighbours.filter(n => n != this && grid.contains(n))
    }
  }

  def sinks(grid: Grid): Seq[Point] =
    grid.keys
        .toSeq
        .filter(p => grid(p) < p.neighbours(grid).map(grid).min)

  def bfs(fn: Point => Seq[Point])(p: Point): LazyList[Point] = {
    def _bfs(q: Queue[Point], s: Set[Point]): LazyList[Point] = q match {
      case p +: ps => p #:: _bfs(ps ++ fn(p).filter(!s.contains(_)), s + p)
      case _       => LazyList.empty
    }

    p #:: _bfs(Queue.empty ++ fn(p), Set.empty)
  }

  def basin(grid: Grid)(sink: Point): Set[Point] = {
    val fn: Point => Seq[Point] = p => p.neighbours(grid).filter(n => grid(n) < 9)
    bfs(fn)(sink).toSet
  }

  def p1(grid: Grid): Int =
    sinks(grid)
      .map(grid)
      .map(_ + 1)
      .sum

  def p2(grid: Grid): Int =
    sinks(grid)
      .map(basin(grid))
      .map(_.size)
      .sorted
      .takeRight(3)
      .product
}

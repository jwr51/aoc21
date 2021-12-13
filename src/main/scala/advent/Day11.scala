package advent

import scala.annotation.tailrec
import scala.io.Source

object Day11:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day11.txt").mkString
    val grid = input
      .linesIterator
      .toList
      .zipWithIndex
      .flatMap((row, y) => row.zipWithIndex.map((c, x) => Point(x, y) -> c.asDigit))
      .toMap

    println(p1(grid))
    println(p2(grid))

  case class Point(x: Int, y: Int) {
    def neighbours(grid: Grid): Seq[Point] =
      val ns =
        for nx <- x - 1 to x + 1
            ny <- y - 1 to y + 1
        yield Point(nx, ny)

      ns.filter(p => p != this && grid.contains(p))
  }

  type Grid = Map[Point, Int]

  def step(grid: Grid): Grid =
    @tailrec
    def fn(grid: Grid, rem: Seq[Point], f: Set[Point]): Grid = rem match
      case Nil                      => grid
      case x :: xs if f.contains(x) => fn(grid, xs, f)
      case x :: xs if grid(x) < 9   => fn(grid.updated(x, grid(x) + 1), xs, f)
      case x :: xs                  => fn(grid.updated(x, 0), xs ++ x.neighbours(grid), f + x)

    fn(grid, grid.keys.toSeq, Set.empty)

  def p1(grid: Grid): Int =
    Iterator
      .iterate(grid)(step)
      .slice(1, 101)
      .flatMap(_.values)
      .count(_ == 0)

  def p2(grid: Grid): Int =
    Iterator
      .iterate(grid)(step)
      .indexWhere(_.values.forall(_ == 0))

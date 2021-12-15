package advent

import advent.Day09.Point
import advent.Day11.{Grid, Point}

import scala.collection.mutable
import scala.io.Source

object Day15:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day15.txt").mkString
    val grid = input
      .linesIterator
      .toList
      .zipWithIndex
      .flatMap((row, y) => row.zipWithIndex.map((c, x) => Point(x, y) -> c.asDigit))
      .toMap

    println(p1(grid))
    println(p2(grid))

  case class Point(x: Int, y: Int):
    def dist(o: Point): Int = (o.y - y).abs + (o.x - x).abs

  type Grid = Map[Point, Int]
  type Path = List[Point]

  extension (grid: Grid)
    def bottomRight: Point = grid.keys.maxBy(p => p.x + p.y)

    def neighbours(p: Point): Seq[Point] =
      val ns =
        for nx <- p.x - 1 to p.x + 1
            ny <- p.y - 1 to p.y + 1
            if nx == p.x || ny == p.y
        yield Point(nx, ny)
      ns.filter(n => n != p && grid.contains(n))

  def astar(grid: Grid): Path =
    val start = Point(0, 0)
    val goal = grid.bottomRight
    val h = goal.dist
    val open = mutable.Set(start)
    val closed = mutable.Set.empty[Point]
    val parent = mutable.Map.empty[Point, Point]
    val f = mutable.Map.empty[Point, Int].withDefaultValue(Int.MaxValue)
    val g = mutable.Map.empty[Point, Int].withDefaultValue(Int.MaxValue)

    while (open.nonEmpty)
      val c = open.minBy(f)
      if c == goal then
        return Iterator
          .iterate(List(c))(p => if p.head == start then p else parent(p.head) :: p)
          .dropWhile(_.head != start)
          .next()
      else
        open.remove(c)
        closed.add(c)

        grid.neighbours(c).filterNot(closed.contains).foreach(n =>
          val tg = g(c) + grid(n)
          if !open.contains(n) || tg < g(n) then
            parent(n) = c
            g(n) = tg
            f(n) = g(n) + h(n)
            if !open.contains(n) then open.add(n)
        )

    List.empty

  def tile(grid: Grid): Grid =
    val dim = grid.bottomRight.x + 1
    grid.keys
        .flatMap(k => (0 to 4).flatMap(y => (0 to 4).map(x => Point(k.x + x * dim, k.y + y * dim))))
        .map(p =>
          if grid.contains(p) then (p, grid(p))
          else {
            val r = grid(Point(p.x % dim, p.y % dim)) + (p.x / dim + p.y / dim)
            (p, if r > 9 then r - 9 else r)
          }).toMap

  def p1(grid: Grid): Int = astar(grid).drop(1).map(grid).sum

  def p2(grid: Grid): Int =
    val tiles = tile(grid)
    astar(tile(grid)).drop(1).map(tiles).sum

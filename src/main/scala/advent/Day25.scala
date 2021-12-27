package advent

import scala.io.Source

object Day25:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day25.txt").getLines().toList

    val (width, height) = (input.head.length, input.size)
    val cucumbers = (for
      y <- 0 until height
      x <- 0 until width
      if input(y)(x) != '.'
    yield Pos(x, y) -> input(y)(x)).toMap

    val ocean = Ocean(width, height, cucumbers)
    val p1 = Iterator.iterate(ocean)(_.update).sliding(2).indexWhere(p => p.head == p.last) + 1
    println(p1)

  case class Pos(x: Int, y: Int)
  case class Ocean(width: Int, height: Int, cucumbers: Map[Pos, Char]):
    def update: Ocean =
      move('>', p => p.copy(x = (p.x + 1) % width)).move('v', p => p.copy(y = (p.y + 1) % height))

    def move(c: Char, fn: Pos => Pos): Ocean =
      val (free, blocked) = cucumbers
        .filter((_, q) => q == c)
        .partition((p, _) => !cucumbers.contains(fn(p)))
      copy(cucumbers = cucumbers.filter((_, q) => q != c) ++ blocked ++ free.map((p, q) => fn(p) -> q))
package advent

object Day1 {

  // https://adventofcode.com/2021/day/1
  def main(args: Array[String]): Unit = {
    val input = Advent.input(1)
    println(p1(input))
    println(p2(input))
  }

  def p1(input: String): Any = {
    input
      .linesIterator
      .map(_.toInt)
      .sliding(2)
      .count(p => p.last > p.head)
  }

  def p2(input: String): Any = {
    input
      .linesIterator
      .map(_.toInt)
      .sliding(3)
      .map(_.sum)
      .sliding(2)
      .count(p => p.last > p.head)
  }
}
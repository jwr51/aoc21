package advent

object Day6 {

  def main(args: Array[String]): Unit = {
    val input = Advent.input(6)
    println(p1(input))
    println(p2(input))
  }

  def parse(input: String): List[Int] = input.split(",").map(_.toInt).toList

  def population(input: List[Int], days: Int): Long = {
    def evolve(state: List[Long]): List[Long] = {
      val spawns :: tail = state
      tail.updated(6, tail(6) + spawns) :+ spawns
    }

    val state: List[Long] = List.tabulate(9)(i => input.count(_ == i))
    Iterator.iterate(state)(evolve).drop(days).next.sum
  }

  def p1(input: String): Any = population(parse(input), days = 80)

  def p2(input: String): Any = population(parse(input), days = 256)
}

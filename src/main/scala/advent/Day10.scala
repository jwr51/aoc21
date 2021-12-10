package advent

import scala.annotation.tailrec

object Day10 {

  def main(args: Array[String]): Unit = {
    val input = Advent.input(10)
    println(p1(input))
    println(p2(input))
  }

  val left = List('[', '<', '{', '(')
  val right = List(']', '>', '}', ')')
  val errorTable = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val unclosedTable = Map('(' -> 1, '[' -> 2, '{' -> 3, '<' -> 4)

  def analyse(line: String): (Option[Int], List[Char]) = {
    @tailrec
    def fn(line: List[Char], open: List[Char], e: Option[Int]): (Option[Int], List[Char]) = e match {
      case None => line match {
        case c :: cs if left.contains(c)                    => fn(cs, c :: open, e)
        case c :: cs if open.head == left(right.indexOf(c)) => fn(cs, open.tail, e)
        case c :: _                                         => (Some(errorTable(c)), open)
        case Nil                                            => (e, open)
      }
      case _    => (e, open)
    }

    fn(line.toList, List.empty, None)
  }

  def error(line: String): Option[Int] = {
    val (error, _) = analyse(line)
    error
  }

  def p1(input: String): Any = input.linesIterator.flatMap(error).sum

  def p2(input: String): Any = {
    val scores = input
      .linesIterator
      .map(analyse)
      .flatMap((e, o) => if e.isEmpty then Some(o) else None)
      .map(_.map(unclosedTable))
      .map(_.foldLeft(0L)((c, s) => c * 5 + s))
      .toList
      .sorted
    scores(scores.length / 2)
  }
}

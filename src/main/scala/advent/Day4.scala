package advent

import scala.annotation.tailrec

object Day4 {

  def main(args: Array[String]): Unit = {
    val input = Advent.input(4)
    println(p1(input))
    println(p2(input))
  }

  type Board = Seq[Seq[Int]]

  def parse(input: String): (Seq[Int], Set[Board]) = {
    val lines = input.linesIterator.toSeq
    val draws = lines.head.split(",").map(_.toInt).toSeq
    val boards = lines.drop(2)
                      .filter(_.nonEmpty)
                      .map(_.trim())
                      .map(_.split("""[^\d]+"""))
                      .flatMap(_.map(_.toInt))
                      .grouped(5)
                      .grouped(5)
                      .toSet
    (draws, boards)
  }

  def wins(b: Board, ns: Seq[Int]): Boolean = (b ++ b.transpose).exists(qs => qs.forall(ns.contains(_)))

  def score(b: Board, ns: Seq[Int]): Int = {
    val last = ns.last
    b.flatten.filter(!ns.contains(_)).sum * last
  }

  def play(bs: Set[Board], nums: Seq[Int]): Seq[Int] = {
    @tailrec
    def _play(bs: Set[Board], drawn: List[Int], rem: List[Int], scores: Seq[Int]): Seq[Int] = rem match {
      case Nil     => scores
      case n :: ns =>
        val nums = drawn :+ n
        val ws = bs.filter(b => wins(b, nums))
        _play(bs -- ws, nums, ns, scores ++ ws.map(b => score(b, nums)))
    }

    _play(bs, List.empty, nums.toList, Seq.empty)
  }

  def p1(input: String): Any = {
    val (nums, boards) = parse(input)
    play(boards, nums).head
  }

  def p2(input: String): Any = {
    val (nums, boards) = parse(input)
    play(boards, nums).last
  }
}

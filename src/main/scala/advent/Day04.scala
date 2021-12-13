package advent

import scala.annotation.tailrec
import scala.io.Source

object Day04:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day04.txt").getLines().toSeq
    val nums = input.head.split(",").map(_.toInt).toSeq
    val boards = input.drop(2)
                      .filter(_.nonEmpty)
                      .map(_.trim())
                      .map(_.split("""[^\d]+"""))
                      .flatMap(_.map(_.toInt))
                      .grouped(5)
                      .grouped(5)
                      .toSet

    println(p1(nums, boards))
    println(p2(nums, boards))

  type Board = Seq[Seq[Int]]

  extension (b: Board)
    def wins(nums: Seq[Int]): Boolean = (b ++ b.transpose).exists(_.forall(nums.contains))
    def score(nums: Seq[Int]): Int = b.flatten.filterNot(nums.contains).sum * nums.head

  def play(bs: Set[Board], nums: Seq[Int]): Seq[Int] =
    @tailrec
    def _play(bs: Set[Board], drawn: List[Int], rem: List[Int], scores: Seq[Int]): Seq[Int] =
      rem match
        case Nil     => scores
        case n :: ns =>
          val nums = n :: drawn
          val ws = bs.filter(b => b.wins(nums))
          _play(bs -- ws, nums, ns, scores ++ ws.map(_.score(nums)))

    _play(bs, List.empty, nums.toList, Seq.empty)

  def p1(nums: Seq[Int], boards: Set[Board]): Int = play(boards, nums).head

  def p2(nums: Seq[Int], boards: Set[Board]): Int = play(boards, nums).last

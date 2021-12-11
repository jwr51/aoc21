package advent

import scala.io.Source

object Day02 {

  // https://adventofcode.com/2021/day/2
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day02.txt").getLines().map(parse).toSeq
    
    println(p1(input))
    println(p2(input))
  }

  sealed trait Command
  case class MoveHorizontal(n: Int) extends Command
  case class MoveVertical(n: Int) extends Command

  private val Pattern = """(forward|down|up) (\d+)""".r

  def parse(s: String): Command = s match {
    case Pattern("forward", n) => MoveHorizontal(n.toInt)
    case Pattern("down", n)    => MoveVertical(n.toInt)
    case Pattern("up", n)      => MoveVertical(-n.toInt)
  }

  case class Point1(x: Int, y: Int)
  case class Point2(x: Int, y: Int, aim: Int)

  def p1(cs: Seq[Command]): Int = {
    val d = cs.foldLeft(Point1(0, 0))((p, c) => c match {
      case MoveHorizontal(n) => p.copy(x = p.x + n)
      case MoveVertical(n)   => p.copy(y = p.y + n)
    })

    d.x * d.y
  }

  def p2(cs: Seq[Command]): Int = {
    val d = cs.foldLeft(Point2(0, 0, 0))((p, c) => c match {
      case MoveHorizontal(n) => p.copy(x = p.x + n, y = p.y + p.aim * n)
      case MoveVertical(n)   => p.copy(aim = p.aim + n)
    })

    d.x * d.y
  }
}

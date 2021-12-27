package advent

import scala.io.Source
import scala.math.max

object Day17:

  def main(args: Array[String]): Unit =
    val Array(left, right, bottom, top) = Source
      .fromResource("day17.txt")
      .mkString.split("""[^-\d]+""")
      .filter(_.nonEmpty)
      .map(_.toInt)
    val target = Target(left, right, bottom, top)

    println(p1(target))
    println(p2(target))

  case class Target(left: Int, right: Int, bottom: Int, top: Int)

  case class Vec2(x: Int, y: Int):
    def +(o: Vec2): Vec2 = Vec2(x + o.x, y + o.y)
    def hits(t: Target): Boolean = (t.left to t.right).contains(x) && (t.bottom to t.top).contains(y)
    def misses(t: Target): Boolean = x > t.right || y < t.bottom
    def pending(t: Target): Boolean = !hits(t) && !misses(t)

  def p1(t: Target): Int = t.bottom.abs * (t.bottom.abs - 1) / 2

  def sum(n: Int): Int = n * (n + 1) / 2

  def launch(t: Target)(v: Vec2): Boolean =
    Iterator
      .iterate((Vec2(0, 0), v))((p, v) => (p + v, Vec2(max(0, v.x - 1), v.y - 1)))
      .map((p, _) => p)
      .dropWhile(_.pending(t))
      .next()
      .hits(t)

  def p2(t: Target): Int =
    val minVx = (0 to Int.MaxValue).indexWhere(sum(_) >= t.left)
    val vs = for vx <- minVx to t.right; vy <- t.bottom to -t.bottom yield Vec2(vx, vy)
    vs.count(launch(t))

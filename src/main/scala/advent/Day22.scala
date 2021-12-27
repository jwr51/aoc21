package advent

import scala.io.Source

object Day22:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day22.txt")
                      .getLines()
                      .map(parse)
                      .toList
    println(p1(input))
    println(p2(input))

  private val Pattern = """(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)""".r

  def parse(s: String): Cube = s match
    case Pattern(on, x1, x2, y1, y2, z1, z2) =>
      Cube(on == "on", Point(x1.toInt, y1.toInt, z1.toInt), Point(x2.toInt, y2.toInt, z2.toInt))

  case class Point(x: Int, y: Int, z: Int)

  case class Cube(on: Boolean, min: Point, max: Point):
    def intersect(o: Cube): Option[Cube] =
      val minI = Point(math.max(min.x, o.min.x), math.max(min.y, o.min.y), math.max(min.z, o.min.z))
      val maxI = Point(math.min(max.x, o.max.x), math.min(max.y, o.max.y), math.min(max.z, o.max.z))
      if minI.x <= maxI.x && minI.y <= maxI.y && minI.z <= maxI.z then Some(Cube(!o.on, minI, maxI))
      else None

    def small: Boolean =
      val ps = List(min.x, min.y, min.z, max.x, max.y, max.z)
      ps.min >= -50 && ps.max <= 50

    val volume: Long = (max.x - min.x + 1).toLong * (max.y - min.y + 1) * (max.z - min.z + 1)

  def count(input: List[Cube]): Long =
    input
      .foldLeft(List.empty[Cube]) { (cs, c) =>
        val updated = cs ::: cs.flatMap(c.intersect)
        if c.on then c :: updated else updated
      }.map(r => r.volume * (if r.on then 1 else -1))
      .sum

  def p1(input: List[Cube]): Long = count(input.filter(_.small))

  def p2: List[Cube] => Long = count
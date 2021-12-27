package advent

import scala.io.Source

object Day20:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day20.txt").getLines().toList
    val algo = input.head.toList
    val image = input.drop(2)
                     .zipWithIndex
                     .flatMap((s, y) => s.zipWithIndex.map((c, x) => Point(x, y) -> c))
                     .toMap
                     .withDefaultValue('.')

    println(p1(image, algo))
    println(p2(image, algo))

  case class Point(x: Int, y: Int):
    def adjacent: Seq[Point] = for
      dy <- -1 to 1
      dx <- -1 to 1
    yield Point(x + dx, y + dy)

  type Image = Map[Point, Char]
  type Algorithm = List[Char]

  def enhance(algo: Algorithm)(image: Image): Image =
    val (min, max) = (image.keys.map(_.x).min, image.keys.map(_.x).max)
    val enhanced = for
      x <- min - 1 to max + 1
      y <- min - 1 to max + 1
      point = Point(x, y)
      square = point.adjacent.map(image)
      section = square.map(c => if c == '#' then 1 else 0).mkString
      idx = Integer.parseInt(section, 2)
    yield point -> algo(idx)

    val default = image(Point(Int.MinValue, Int.MinValue))
    val idx = if default == '#' then 511 else 0
    enhanced.toMap.withDefaultValue(algo(idx))

  def p1(image: Image, algo: Algorithm): Int =
    Iterator.iterate(image)(enhance(algo)).drop(2).next().values.count(_ == '#')

  def p2(image: Image, algo: Algorithm): Int =
    Iterator.iterate(image)(enhance(algo)).drop(50).next().values.count(_ == '#')
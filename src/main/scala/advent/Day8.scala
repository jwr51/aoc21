package advent

object Day8 {

  def main(args: Array[String]): Unit = {
    val input = Advent.input(8)
    println(p1(input))
    println(p2(input))
  }

  def p1(input: String): Any = input.linesIterator
                                    .flatMap(_.split(" ").takeRight(4))
                                    .map(_.length)
                                    .count(l => l < 5 || l == 7)

  /*def decode(line: String): Int = {
    val keys = List("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg")
    val maps = "abcdefg".permutations.map(_.zip("abcdefg")).map(_.toMap).toSeq

    val outs = maps.flatMap(cs => {
      val mapped = line.map(c => cs.getOrElse(c, c))
      val words = mapped.split(" ").map(_.sorted).toSeq
      val (signal, output) = (words.take(10), words.takeRight(4))

      if signal.sorted != keys.sorted then None else Some(output.map(keys.indexOf).mkString.toInt)
    })

    if outs.size == 1 then outs.head else throw new Error("no unique solution")
  }*/

  def decode(line: String): Int = {
    val words = line.split(" ").map(_.sorted).toSeq
    val (signal, keys) = (words.take(10), words.takeRight(4))

    def common(s: String, len: Int): Int = s.intersect(signal.find(_.length == len).get).length

    keys.map(k => k.length match {
      case 2                            => 1
      case 3                            => 7
      case 4                            => 4
      case 5 if common(k, len = 3) == 3 => 3 // contains 7
      case 5 if common(k, len = 4) == 3 => 5 // contains 3/4 chars from 4
      case 5                            => 2
      case 6 if common(k, len = 4) == 4 => 9 // contains 4
      case 6 if common(k, len = 3) == 3 => 0 // contains 7
      case 6                            => 6
      case 7                            => 8
    }).mkString.toInt
  }

  def p2(input: String): Any = input.linesIterator.map(decode).sum
}

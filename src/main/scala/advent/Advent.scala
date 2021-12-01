package advent

object Advent {

  def input(day: Int): String = read(s"/day$day.txt")

  def read(s: String): String = {
    val bytes = getClass.getResourceAsStream(s).readAllBytes()
    new String(bytes)
  }
}
object Main {

  def main(args: Array[String]): Unit = {
    var n = readLine().toInt
    while (n > 0) {
      val time = eachLine(n)
        .map(parseLine)
        .filter(_._2 > 0)
        .map {
          case (v, t) => t + 4.5/(v/3600.0)
        }.min
      println(Math.ceil(time).toInt)
      n = readLine().toInt
    }
  }

  /** Splits given line into (int, int). */
  private[this] def parseLine(l: String) = {
    val nums = l.stripLineEnd.split('\t').map(_.toDouble)
    (nums(0), nums(1))
  }

  private[this] def eachLine(n: Int): Seq[String] =
    (1 to n).map { _ => readLine() }
}

object main {
  def main(args: Array[String]) {
    eachLine(_.stripLineEnd.toDouble)
      .foreach { in =>
        var n = in
        var i = 1
        while(n > 0) {
          i += 1
          n -= 1.0/i
        }
        println(s"${i - 1} card(s)")
      }
  }

  private[this] def eachLine(f: String => Double): Iterator[Double] =
    Iterator.continually(readLine()).takeWhile(defined).map(f)

  def defined(l: String) = null != l && l.nonEmpty && l != "0.00"
}

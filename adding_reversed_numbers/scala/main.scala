object main {
  def main(args: Array[String]) {
    readLine() // skip first line
    Iterator.continually(Console.readLine).takeWhile(defined).foreach {
      line =>
        val pair = line.stripLineEnd.split(' ')
        // nicer, modular way
        println(reverse_sum(pair(0), pair(1)))
        // faster, one-liner way
        // println(line.stripLineEnd.reverse.split(' ').map(_.toInt).sum.toString.reverse.toInt)
    }
  }

  def defined(l: String) = null != l && l.nonEmpty

  def reverse_sum(a: Int, b: Int): Int = reverse(reverse(a) + reverse(b))
  def reverse_sum(a: String, b: String): Int = reverse_sum(a.toInt, b.toInt)
  def reverse(x: Int): Int = x.toString.reverse.toInt
  def reverse(s: String): Int = reverse(s.toInt)
}

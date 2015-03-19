import scala.io.StdIn.readLine

object main {

  def main(args: Array[String]): Unit = {
    val numCases = readLine().stripLineEnd.toInt
    1 to numCases foreach { _ =>
      readLine()
      val fragments =
        Iterator
            .continually(readLine())
            .takeWhile(defined)
            .map(_.stripLineEnd).toIndexedSeq
      println(solve(fragments))
      println()
    }
  }

  def defined(s: String) = null != s && s.nonEmpty

  /** @return complete file
    */
  def solve(fragments: IndexedSeq[String]): String = {

    // TODO use bucket sort
    val a = fragments.sortWith((l, r) => l < r)
    val b = fragments.sortWith((l, r) => l.reverse < r.reverse)

    val a1 = a(0) // first on the left
    val an = a(a.length - 1) // last on the left
    val b1 = b(b.length - 1) // last on right
    val bn = b(0) // first on the right

    // (a1, b1) and (an, bn) are pairs
    // determine if a1 + b1 or b1 + a1 is the solution
    val cand1 = List(a1 + b1, b1 + a1)
    val candn = List(an + bn, bn + an)
    cand1.find(c1 => candn.contains(c1)).get

    // TODO proof
  }

  def measure(fragments: Seq[String]): Int = {
    val count = fragments.length / 2
    fragments.map(f => f.length).sum / count
  }
}

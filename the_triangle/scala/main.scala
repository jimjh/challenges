object main {

  type Row = IndexedSeq[Int]

  private[this] def parse(l: String)(down: Row): Row =
    l.split(' ').zipWithIndex.map {
      case (node, i) =>
        val left = down(i)
        val right = down(i+1)
        Math.max(left, right) + node.toInt
    }

  private[this] def zeroArray(len: Int) =
    Array.fill(len)(0)

  private[this] val max = parse _

  def solve(read: () => String): Int = {
    var k = identity[Row](_)
    val n = read().toInt
    for (i <- 1 to n) {
      val line = read()
      k = max(line) andThen k
    }
    k(zeroArray (n+1))(0)
  }

  def main(args: Array[String]) {
    println(solve { () => readLine().stripLineEnd })
  }

}

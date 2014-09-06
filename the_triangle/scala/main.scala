object main {

  private[this] def parse(line: String)(children: IndexedSeq[Int]): IndexedSeq[Int] = {
    val nums = line.stripLineEnd.split(' ').map(_.toInt)
    val len = nums.size
    val arr = new Array[Int](len)
    for (i <- 0 to len-1) {
      val left = children(i)
      val right = children(i+1)
      arr(i) = Math.max(left, right) + nums(i)
    }
    arr
  }

  private[this] def zeroArray(len: Int) =
    Array.fill(len)(0)

  private[this] val max = parse _

  def solve(read: () => String): Int = {
    var k = identity[IndexedSeq[Int]](_)
    val n = read().stripLineEnd.toInt
    for (i <- 1 to n) {
      val line = readLine()
      k = max(line) andThen k
    }
    k(zeroArray (n+1))(0)
  }

  def main(args: Array[String]) {
    println(solve(readLine))
  }

}

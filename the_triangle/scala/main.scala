import scalaz._, Scalaz._, effect._, IO._

object main {

  type Row = IndexedSeq[Int]

  private[this] def parse(l: String)(down: Row): Row =
    l.split(' ').zipWithIndex.map {
      case (node, i) =>
        val left = down(i)
        val right = down(i+1)
        Math.max(left, right) + node.toInt
    }

  private[this] def zeroArray(len: Int) = Array.fill(len)(0)
  private[this] val max = parse _
  private[this] val ident = identity[Row] _

  def readLnN(n: Int): IO[IndexedSeq[String]] = IO {
    (1 to n).map { _ => readLine() }
  }

  def program = {
    val n: IO[Int] = readLn.map(_.toInt)
    val lines: IO[IndexedSeq[String]] = n.flatMap(readLnN)
    val ans = lines.map { ls =>
      val cont = ls.foldLeft(ident) {
        (fun, line) => max(line) andThen fun
      }
      cont(zeroArray (ls.size+1))(0)
    }
    ans.flatMap { i:Int => putStrLn(i.toString) }
  }

  def main(args: Array[String]) {
    program.unsafePerformIO
  }

}

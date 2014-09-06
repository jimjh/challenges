import scalaz._, Scalaz._, effect._, IO._

object main {

  type Row = IndexedSeq[Int]

  private[this] def zeroArray(len: Int) = Array.fill(len)(0)
  private[this] val ident = identity[Row] _

  /** Solve for all nodes on this row, given results from the row below. */
  private[this] def parse(l: String)(down: Row): Row =
    l.split(' ').zipWithIndex.map {
      case (node, i) =>
        val left = down(i)
        val right = down(i+1)
        Math.max(left, right) + node.toInt
    }

  /** Syntax sugar for parse, so I can partially apply it. */
  private[this] val max = parse _

  def readLnN(n: Int): IO[IndexedSeq[String]] = IO {
    1 to n map { _ => readLine() }
  }

  def solve(lines: IndexedSeq[String]) = {
    val cont = (ident /: lines) {
      (f, l) => max(l) andThen f
    }
    IO { cont(zeroArray (lines.size+1))(0) }
  }

  def program = {
    for {
      n <- readLn
      lines <- readLnN(n.toInt)
      ans <- solve(lines)
      _ <- putLn(ans)
    } yield()
  }

  def main(args: Array[String]) {
    program.unsafePerformIO
  }

}

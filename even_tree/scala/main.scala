object main {

  def main(args: Array[String]) {
    val (n, m) = parseFirstLine()
    val graph = buildGraph(n)
  }

  def defined(l: String) = null != l && l.nonEmpty

  def parseFirstLine() = {
    val pair = readLine().stripLineEnd.split(' ')
    (pair(0).toInt, pair(1).toInt)
  }

  def buildGraph(n: Int) = {
    val graph = new Graph(n)
    Iterator.continually(Console.readLine).takeWhile(defined).foreach {
      line =>
        val pair = line.stripLineEnd.split(' ')
        graph.connect(pair(0).toInt, pair(1).toInt)
    }
  }

  case class Node(weight: Int = 1, parent: Option[Node] = None)

  class Graph(n: Int) {
    val nodes = (1 until n).map { i => Node() }
    def connect(src: Int, dst: Int) {
      nodes(src) = Node(1,  // hmmm parent?
    }
  }

  // keep track of leaves
  // for each leaf
  //   if is odd, collapse into parent
  //   if is even, break off from parent, increment removed_edges

}

/* FUN THINGS TO DO
 * - make this functional
 * - make this parallel
 */
import collection._

import Console.err

object main {

  def main(args: Array[String]) {
    val (n, _) = readSize()
    val graph = buildGraph(n)

    var removed = 0
    var count = 0
    do {
      graph.collapse()
      removed = graph.truncate()
      count += removed
    } while (removed > 0)

    print(count)
  }

  /** Parses the first line of input and returns |V| and |E|. */
  def readSize() =
    parseLine(readLine())

  /** Parses input lines and constructs the graph from given edges. */
  def buildGraph(n: Int) = {
    val graph = new Graph
    eachLine(parseLine).foreach {
      case (u, v) => graph.addEdge(u, v)
    }
    graph
  }

  private[this] def defined(l: String) = null != l && l.nonEmpty

  private[this] def eachLine(f: (String) => (Int, Int)): Iterator[(Int, Int)] =
    Iterator.continually(readLine()).takeWhile(defined).map(f)
    // io.Source.fromFile("even_tree/mod-input.txt").getLines().map(f)

  /** Splits given line into (int, int). */
  private[this] def parseLine(l: String) = {
    val nums = l.stripLineEnd.split(' ').map(_.toInt)
    (nums(0), nums(1))
  }

  class Node(val id: Int,
             val neighbors: mutable.Set[Int] = mutable.Set[Int](),
             var weight: Int = 1) {
    override def toString =
      s"[$id] weight: $weight, neighbors: $neighbors"
    def isLeaf = 1 == neighbors.size
    def isEven = 0 == weight % 2
  }

  class Graph() {
    val nodes = mutable.Map[Int, Node]()

    def addEdge(u: Int, v: Int) = {
      val nodeU = nodes getOrElseUpdate (u, new Node(u))
      val nodeV = nodes getOrElseUpdate (v, new Node(v))
      nodeU.neighbors += v
      nodeV.neighbors += u
      (u, v)
    }

    // TODO keep track of leaves
    /** Collapse leaves into their neighbors. */
    def collapse() = {
      nodes.filter(_._2.isLeaf).foreach {
        case (u, node) =>
          err.println("found leaf " + u)
          node.neighbors.foreach {
            v =>
              err.println("> collapsing into " + v)
              val neighbor = nodes(v)
              neighbor.weight += node.weight
              assert(neighbor.neighbors remove u)
          }
          assert(nodes.remove(u).nonEmpty) // kill
      }
    }

    /** Disconnect even components. */
    def truncate() = {
      var removed = 0
      var delta = 0
      do {
        delta = 0
        nodes
          .filter(_._2.isLeaf)
          .filter(_._2.isEven)
          .foreach {
            case (u, node) =>
              if (node.neighbors.size > 0)
                err.println("detaching node " + u)
              delta += node.neighbors.size
              node.neighbors.foreach {
                v =>
                  err.println("> from " + v)
                  val neighbor = nodes(v)
                  assert(neighbor.neighbors remove u)
              }
              assert(nodes.remove(u).nonEmpty)
          }
        removed += delta
      } while (0 < delta)

      removed
    }
  }
}

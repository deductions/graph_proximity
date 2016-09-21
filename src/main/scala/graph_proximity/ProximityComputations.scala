package graph_proximity

import algorithm.graph.IPath
import algorithm.graph.ChangableWeightedDirectedGraph
import algorithm.graph.TopKShortestPaths
import algorithm.graph.INode

/**
 */
trait ProximityComputations[T] {
  /**
   *  The weight of edge (i, j) ∈ E is denoted by w ij > 0 and reflects the similarity of i and j
   *  (higher weights reflect higher similarity).
   *
   *  Currently we set this to 1, because the underlying algorithm for k shortest path has no weight for edges.
   */
  val specificEdgeWeight = 1.0
  type Node = INode[T]
  type Path = IPath[T]

  def proximity(start: T, end: T, k: Int,
                graph: ChangableWeightedDirectedGraph[T]): Double = {
    val finder = new TopKShortestPaths[T](graph)
    val paths = finder.findPaths(start, end, k)

    /**
     *  The probability of transition for a random walk.
     */
    def pathWeight(path: Path): Double = {
      val nodes = path.nodeList
      if (nodes.isEmpty) return 0

      val firstNode = nodes(0)
      /* this trick is (hopefully) more efficient than calling everytime:
      val n = nodes.indexOf(node); nodes.apply(n+1) */
      val node_following_pairs = (firstNode +: nodes) zip nodes
      val edgeWeights = for (
        (node, following) <- node_following_pairs if node != firstNode
      ) yield { edgeWeight(node, following) }

      println(s"edgeWeights $edgeWeights ")

      edgeWeights.foldLeft(1.0) {
        (acc, w) =>
          acc * w } *
        degree(firstNode)
    }

    /**
     *  The probability of transition for a random walk.
     */
    def edgeWeight(node: Node, following: Node): Double = {
      specificEdgeWeight / degree(node)
    }

    /**
     * Each node is associated with a degree equal to the sum of the edge weights coming
     *  out of that node.
     */
    def degree(node: Node): Double = {
      val outNodes = graph.fanOut(node)
      outNodes.foldLeft(0.0) {
        (acc, nod) =>
          println( s"nod $nod ${nod.weight}" )
          acc + nod.weight
      }
    }

    val pathWeights = for (path <- paths) yield {
      val pw = pathWeight(path)
    	println( s"path $path pathWeight $pw")
      pw
    }

    pathWeights.foldLeft(0.0) {
      (acc, w) =>
        println( s"acc $acc + $w")
        acc + w
    }
  }

}
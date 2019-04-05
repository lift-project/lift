package utils

import scala.collection.mutable.ListBuffer

/* Topological sort of directed graph using Kahn's algorithm.
*  Throws an exception is the graph is not acyclic.
*  Used for parameter space exploration to determine the order of parameter value generation based on dependencies
*  in parameter validation rules
*/
object GraphSort {

  /**
    * NB: edges can have duplicates
    */
  def topologicalSort(vertices: Vector[Int], edges: Array[(Int, Int)],
                      circularDependenciesAllowed: Boolean = false): List[Int] = {
    val adjacency: Array[Array[Boolean]] = Array.fill(vertices.length, vertices.length)(false)

    for (edge <- edges) adjacency(edge._1/*from*/)(edge._2/*to*/) = true
    var result = ListBuffer[Int]()
    var unsortedVertices: Set[Int] = vertices.toSet
    var verticesWithoutDependencies: Set[Int] = Set()

    while (unsortedVertices.nonEmpty) {
      verticesWithoutDependencies = Set()
      var vertexWithLeastDependencies: Option[Int] = None
      var minDependencies: Int = Int.MaxValue

      for (vertexIdx <- unsortedVertices) {
        val vertexDependencyCount = countDependencies(vertexIdx, unsortedVertices, adjacency)

        if (vertexDependencyCount < minDependencies) {
          vertexWithLeastDependencies = Some(vertexIdx)
          minDependencies = vertexDependencyCount
        }

        if (vertexDependencyCount == 0) {
          verticesWithoutDependencies += vertexIdx
          result += vertices(vertexIdx)
        }
      }
      if (verticesWithoutDependencies.isEmpty) {
        // The graph has cycles. Return the vertex with least dependencies
        vertexWithLeastDependencies match {
          case Some(vertexIdx) =>
            if (circularDependenciesAllowed) {
              verticesWithoutDependencies += vertexIdx
              result += vertices(vertexIdx)
            } else
              throw new IllegalArgumentException(f"Circular dependencies are not allowed. Detected a dependency " +
                f"${unsortedVertices.mkString("(", ", ", ")")}")

          case None =>
            throw new IllegalArgumentException("No unsorted vertices left")
        }
      }

      unsortedVertices = unsortedVertices.diff(verticesWithoutDependencies)
    }
    result.toList
  }

  def countDependencies(vertexIdx: Int, unsortedVertices: Set[Int], adjacency: Array[Array[Boolean]]): Int =
    unsortedVertices.count(v => adjacency(vertexIdx)(v))

}
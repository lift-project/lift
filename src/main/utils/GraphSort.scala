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
  def topologicalSort(vertices: Vector[Int], edges: Array[(Int, Int)]): List[Int] = {
    val adjacency: Array[Array[Boolean]] = Array.fill(vertices.length, vertices.length)(false)

    for (edge <- edges) adjacency(edge._1/*from*/)(edge._2/*to*/) = true
    var result = ListBuffer[Int]()
    var unsortedVertices: Set[Int] = vertices.toSet
    var verticesWithoutDependencies: Set[Int] = Set()

    while (unsortedVertices.nonEmpty) {
      verticesWithoutDependencies = Set()
      for (vertexIdx <- unsortedVertices) {
        if (!hasDependency(vertexIdx, unsortedVertices, adjacency)) {
          verticesWithoutDependencies += vertexIdx
          result += vertices(vertexIdx)
        }
      }
      if (verticesWithoutDependencies.isEmpty)
        throw new IllegalArgumentException("The graph has cycles")
      else
        unsortedVertices = unsortedVertices.diff(verticesWithoutDependencies)
    }
    result.toList
  }

  def hasDependency(vertexIdx: Int, unsortedVertices: Set[Int], adjacency: Array[Array[Boolean]]): Boolean = {
    for (v <- unsortedVertices) {
      if (adjacency(vertexIdx)(v)) return true
    }
    false
  }
}
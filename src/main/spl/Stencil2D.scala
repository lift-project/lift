package spl

import ir.ast.{Slide2D, Lambda, Pad2D, Pad}

/*
object Stencil2D {
  def apply(neighbors: Array[Int], boundary: Pad.BoundaryFun): Lambda = {
    Group2D(neighbors) o Pad2D(neighbors.map(Math.abs).max, boundary)
  }
}
*/

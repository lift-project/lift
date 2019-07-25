package ir.ast

import ir.{ArrayType, Type}
import lift.arithmetic.{ArithExpr, Cst}

package object onnx {
  trait ONNXPattern

  trait Broadcastable {
    def tryBroadcastingShapes(typeA: ArrayType, typeB: ArrayType): Option[Seq[ArithExpr]] = {
      val (shape1, shape2) = {
        // Find a longer shape
        val (longerShape, shorterShape) = {
          val shapeA = Type.getLengths(typeA)
          val shapeB = Type.getLengths(typeB)
          if (shapeA.length >= shapeB.length) (shapeA, shapeB) else (shapeB, shapeA)
        }
        // Prepend the shorter shape with lengths of the longer shape; replace all 1s with the corresponding lengths
        // from the other shape. We are doing this because during broadcasting this will happen anyway.
        // See https://github.com/onnx/onnx/blob/master/docs/Broadcasting.md
        (longerShape.zipWithIndex.map(pair =>
          if (pair._2 < shorterShape.length) {
            if (longerShape(pair._2) == Cst(1)) shorterShape(pair._2)
            else longerShape(pair._2)
          }
          else longerShape(pair._2)
        ),
          longerShape.zipWithIndex.map(pair =>
            if (pair._2 < shorterShape.length) {
              if (shorterShape(pair._2) == Cst(1)) longerShape(pair._2)
              else shorterShape(pair._2)
            }
            else longerShape(pair._2)
          ))
      }


      if (shape1.equals(shape2)) Some(shape1)
      else None
    }
  }
}

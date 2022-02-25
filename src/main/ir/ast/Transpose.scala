package ir.ast

import ir.interpreter.Interpreter.ValueMap
import ir._
import lift.arithmetic.Var
import org.junit.Test

/**
 * Transpose pattern. Performs the transpose on the next read.
 * Code for this pattern can be generated.
 *
 * Equivalent to Split(N) o Gather(IndexFunction.transposeFunction(M, N)) o Join() when applied to type
 * ArrayType(ArrayType( ..., M), N) but infers N and M automatically during view generation.
 *
 * The transpose pattern has the following high-level semantics:
 * `Transpose()([ [x,,1,,, ..., x,,n,,], [y,,1,,, ..., y,,n,,], ..., [z,,1,,, ..., z,,n,,] ]) = [ [x,,1,,, y,,1,,, ..., z,,1,,], [x,,2,,, y,,2,,, ..., z,,2,,], ..., [x,,n,,, y,,n,, ..., z,,n,,,] ]`
 *
 * The transpose pattern has the following type:
 * `Transpose() : [ [a],,I,, ],,J,, -> [ [a],,J,, ],,I,,`
 */
case class Transpose() extends Pattern(arity = 1) {


  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayTypeWSWC(ArrayTypeWSWC(t, ns,nc), ms,mc) => ArrayTypeWSWC(ArrayTypeWSWC(t, ms,mc), ns,nc)
      case _ => throw new TypeException(argType, "ArrayType(ArrayType(_,_),_)", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[Vector[_]] = {
    assert(args.length == arity)
    args.head match {
      case vec: Vector[Vector[_] @unchecked] => vec.transpose
    }
  }
}

object Transpose {
  /**
   * Generates an expression that reorders dimensions in an array according to the passed list of dimensions.
   * For example, an order List(0, 2, 1, 3) generates the following expression:
   * (p109 -> Map((p87 -> Map((p17 -> Transpose() $ p17)) $ p87)
   *  ) o Map((p38 -> Transpose() $ p38 )) $ p109 )
   *
   * @param axes New order of dimensions. The outermost dimension corresponds to 0.
   * @return
   */
  def apply(axes: Int*): FunDecl = {
    if (axes.isEmpty)
      return fun(p => p)

    // Sanity check
    assert(axes.min == 0)
    // Make sure that all dimensions are present in the list
    assert(axes.max == axes.length - 1)
    assert(axes.distinct.length == axes.length /* no duplicates */)

    reorder(axes, Transpose())._1
  }

  def reorder(axes: Seq[Int], transposer: Pattern = Transpose()): (FunDecl, List[Int]) = {
    val identity = fun(p => p)
    axes.zipWithIndex.foldLeft((identity, axes.indices.toList)){
      case ((intermFunDecl: FunDecl, intermAxes: List[Int]),
      (newAxis: Int, axisId: Int)) =>
        val axisDistance = intermAxes.indexOf(newAxis) - axisId
        assert(axisDistance >= 0)
        if (axisDistance == 0)
          (intermFunDecl, intermAxes)
        else {
          val intermFunDeclUpd = GenerateIR.wrapInMaps(
            GenerateIR.applyInEveryDimUntilDimReverse(transposer, axisDistance), axisId
          ) o intermFunDecl
          val intermAxesUpd = intermAxes.slice(0, axisId) ++ List(newAxis) ++ intermAxes.slice(axisId, axisId + axisDistance) ++
            intermAxes.slice(axisId + axisDistance + 1, intermAxes.length)

          (intermFunDeclUpd, intermAxesUpd)
        }
    }
  }

  def reorderAndRestoreDims(order: Seq[Int], useTransposeW: Boolean): (Lambda, Lambda) = {
    val finalDimOrder = order.indices.map(i => order.indexOf(i))

    (Transpose(order: _*),
      if (useTransposeW) TransposeW(finalDimOrder: _*) else Transpose(finalDimOrder: _*))
  }

  /**
   * Test of the reorder function
   */
  def main(args: Array[String]): Unit = {
    Seq(0, 1, 2, 3).permutations.foreach(axes => {
      println(axes.toString + ": ")
      val (funDecl, order) = reorder(axes)
      val a0 = Var("a0")
      val a1 = Var("a1")
      val a2 = Var("a2")
      val a3 = Var("a3")

      val lambda = fun(ArrayType(ArrayType(ArrayType(ArrayType(opencl.ir.Float, a3), a2), a1), a0), p => funDecl(p))
      println(funDecl)
      TypeChecker(lambda)
      println("Ordr: " + order)
      val dimsInType = Type.getLengths(lambda.body.t).reverse.tail.reverse.map(_.asInstanceOf[Var].name.tail).toString
      println("Type: " + dimsInType)
      assert(order.toString.equals(dimsInType))
      println()
    })
  }
}
package ir

import ir.ast._
import lift.arithmetic.{ArithExpr, Var}
import opencl.ir.pattern.MapSeq

object GenerateIR{

  def applyNTimes(f: => Lambda, n: Int): Lambda = {
    assert(n >= 1)
    if (n == 1) f
    else applyNTimes(f, n - 1) o f
  }

  // 0 -> f, 1 -> Map(f), 2 -> Map(Map(f)), ...
  def wrapInMaps(f: => Lambda, count: Int): Lambda = {
    assert(count >= 0)
    if(count < 1) f
    else Map(wrapInMaps(f, count-1))
  }

  // creates Map(...(Map(f)...) o ... o Map(f) o f
  def applyInEveryDimUntilDim(f: => Lambda, dim: Int): Lambda = {
    if(dim <= 1) f
    else applyInEveryDimUntilDim(Map(f), dim-1) o f
    //else Map(applyInEveryDimUntilDim(f)(dim-1)) o f <- fused-maps version
  }

  // creates f o Map(f) o ... o Map(...(Map(f)...)
  def applyInEveryDimUntilDimReverse(f: => Lambda, dim: Int): Lambda = {
    if(dim <= 1) f
    else f o applyInEveryDimUntilDimReverse(Map(f), dim-1)
    //else f o Map(applyInEveryDimUntilDimReverse(f)(dim-1)) <- fused-maps version
  }

  // like wrapInMaps, but also takes care of tuples
  def wrapInMapsWithTuples(f: => Lambda, t: Type): Lambda = {
    t match {
      case vt: VectorType  => throw new NotImplementedError()
      case tt: TupleType   => val param = Param()
                              Lambda(Array(param), Tuple(tt.elemsT.zipWithIndex.map {
                                case (elemT, elemIdx) => wrapInMapsWithTuples(f, elemT) $ Get(param, elemIdx)}: _*))
      case at: ArrayType   => Map(wrapInMapsWithTuples(f, at.elemT))
      case _ => f
    }
  }

  // [a][A][b][B][c][C]... => [a][b][c]...[A][B][C]...
  def interleaveDimensions(count: Int, i: Int): Lambda = {
    val howManyMaps = -2 * (count - 1 - i) - 1
    if(i < 2) throw new IllegalArgumentException("Not enough dimensions to interleave")
    if(count == 2) GenerateIR.wrapInMaps(Transpose(), howManyMaps)
    else {
      GenerateIR.applyInEveryDimUntilDim(GenerateIR.wrapInMaps(Transpose(), howManyMaps), count - 1) o
        interleaveDimensions(count - 1, i)
    }
  }

  def interleaveDimensionsReverse(dim: Int): Lambda = {
    if(dim < 2) throw new IllegalArgumentException("Not enough dimensions to interleave")
    if(dim == 2) Map(Transpose())
    else interleaveDimensionsReverse(dim -1) o
      GenerateIR.applyInEveryDimUntilDim(GenerateIR.wrapInMaps(Transpose(), dim -1), dim-1)
  }

  def reshape(expr: Expr, oldT: Type, newT: Type): Expr = {
    assert(Type.getAllocatedSize(oldT) == Type.getAllocatedSize(newT))

    if (oldT == newT) expr
    else {
      val oldLengths = Type.getLengths(oldT)
      // Flatten
      val flatExpr = if (oldLengths.length <= 2) expr
        else applyNTimes(Join(), oldLengths.length - 2) $ expr

      // Skip the first length (it'll appear automatically) and the last length of 1
      val newLengths = Type.getLengths(newT).tail.reverse.tail.reverse
      // Create new structure
      newLengths.reverse.foldLeft(flatExpr) {
        case (intermExpr: Expr, len: ArithExpr) => Split(len) $ intermExpr
      }
    }
  }

  def optionalZip(args: Expr*): Expr = {
    if (args.length > 1) Zip(args: _*)
    else args.head
  }

  case class Untangling(arrayGetters: Seq[Lambda],
                        arrayTypes: Seq[Type],
                        retanglingLambda: Lambda)

  // untangleZippedArrays(Arr(TupleT(Float, Float), N)) => (semantically)
  //   Untangling(arrayGetters = Seq(Map(p => p._1), Map(p => p._2)),
  //     arrayTypes = Seq(Arr(Float, N), Arr(Float, N)),
  //     retanglingLambda = fun(p => Zip(p._1, p._2)))
  //   More precisely:
  //   Untangling(arrayGetters = (same), arrayTypes = (same),
  //     retanglingLambda = fun(p => Map(p2 => Tuple(p2._1, p2._2)) $ Zip(p._1, p._2)))

  // untangleZippedArrays(Arr(TupleT(Float, Arr(TupleT(Float, Float), M)), N)) => (semantically)
  //   Untangling(arrayGetters = Seq(Map(p => p._1),
  //                                 Map(p => Map(p2 => p2._1) $ p._2),
  //                                 Map(p => Map(p2 => p2._2) $ p._2)),
  //     arrayTypes = Seq(Arr(Float, N), Arr(Arr(Float, M), N), Arr(Arr(Float, M), N)),
  //     retanglingLambda = fun(p => Map(p2 => Tuple(p2._1, Zip(p2._2, p2._3)))
  //                                 $ Zip(p._1, p._2, p._3)))
  //   More precisely:
  //   Untangling(arrayGetters = (same), arrayTypes = (same),
  //     retanglingLambda = fun(p => Map(p2 => Tuple(p2._1, fun(p3 => Map(p4 => Tuple(p4._1, p4._2)) $ Zip(p2._2, p2._3)) $ p2._2))
  //                                 $ Zip(p._1, p._2, p._3)))
  def untangleZippedArrays(t: Type): Untangling = {
    t match {
      case _: ScalarType | _: VectorType  => Untangling(Seq(fun(p => p)), Seq(t), fun(p => p))
      case tt: TupleType   =>
        val furtherUntanglings = tt.elemsT.map(untangleZippedArrays)

        val paramForArrayRetangling = Param()

        Untangling(arrayGetters = furtherUntanglings.zipWithIndex.flatMap { case (furtherUntangling, i) =>
          furtherUntangling.arrayGetters.map(furtherGetter => furtherGetter o Get(i)) },
          arrayTypes = furtherUntanglings.flatMap(_.arrayTypes),
          retanglingLambda = Lambda(Array(paramForArrayRetangling),
            Tuple(furtherUntanglings.zipWithIndex.map { case (furtherUntangling, i) =>
              furtherUntangling.retanglingLambda $ Get(paramForArrayRetangling, i)
            }: _*)
          ))

      case at: ArrayType   =>
        val furtherUntangling = untangleZippedArrays(at.elemT)

        val paramForArrayRetangling = Param()

        Untangling(
          arrayGetters = furtherUntangling.arrayGetters.map(furtherGetter => fun(p => Map(furtherGetter) $ p)),
          arrayTypes = furtherUntangling.arrayTypes.map(elemT => at.copy(elemT)),
          retanglingLambda = Lambda(Array(paramForArrayRetangling),
            Map(furtherUntangling.retanglingLambda) $ optionalZip(furtherUntangling.arrayGetters.zipWithIndex.map {
              case (_, i) => Get(paramForArrayRetangling, i)}: _*)))
      case _ => throw new NotImplementedError()
    }
  }

  def main(args: Array[String]): Unit = {
    def Arr = ArrayType
    val N = Var("N")
    val M = Var("M")

    val test1 = untangleZippedArrays(Arr(TupleType(opencl.ir.Float, opencl.ir.Float), N))
    val test2 = untangleZippedArrays(Arr(TupleType(opencl.ir.Float, Arr(TupleType(opencl.ir.Float, opencl.ir.Float), M)), N))
    println(test1)
    println(test2)
  }
}

package backends.spatial.accel.ir.pattern

import ir.ast.{FPattern2, IRNode, Lambda1, Lambda2, Pattern}
import ir.interpreter.Interpreter.ValueMap
import ir.{ArrayType, ArrayTypeWS, Memory, ScalarType, TupleType, Type, TypeChecker, TypeException, UnallocatedMemory, UndefType}
import lift.arithmetic.{ArithExpr, SimplifiedExpr, Var}

abstract class AbstractSpFold(val fMap: Lambda1,
                              val fReduce: Lambda2,
                              val chunkSize: ArithExpr,
                              val stride: ArithExpr,
                              val factor: ArithExpr) extends Pattern(arity = 2) with FPattern2 {
  def f1 = fMap
  def f2 = fReduce
  var mapLoopVar: Var = Var("i")
  var reduceLoopVar: Var = Var("j")

//  var accumulatorMem: Memory = UnallocatedMemory
  // Since this is a macro pattern (Reduce and Map), the node needs to have more
  // information than usual nodes, such as fMapT below.
  var flatMapT: Type = UndefType // The type of the Map lambda
  var fMapMem: Memory = UnallocatedMemory // The implicit memory of the Map

  assert(fMap.params.length == 1)
  assert(fReduce.params.length == 2)

  val iterationCount: ArithExpr with SimplifiedExpr = mapLoopVar.range.numVals // == reduceLoopVar.range.numVals
  var shouldUnroll = false

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case TupleType(initT, ArrayTypeWS(elemT, argSize)) =>
        // map input array element type
        fMap.params(0).t = ArrayType(elemT, chunkSize)

        val tiledMapBodyType = TypeChecker.check(fMap.body, setType) // check the body

        val outerSize = (argSize - (chunkSize - stride)) /^ stride

        flatMapT = tiledMapBodyType match {
          // These two cases express two different types of Fold:
          // 1. One with batch fMap function which returns an array for each chunk; the resulting 2D array is flattened
          case ArrayTypeWS(elemT, newChunkSize) =>  ArrayType(elemT, newChunkSize * outerSize)
          // 2. One with batch fMap function which returns a scalar for each chunk; the result is 1D array
          case scalarType: ScalarType =>            ArrayType(scalarType, outerSize)

          case t => throw new TypeException(t, "ArrayTypeWS(_, _) | ScalarType", this)
        }

        fReduce.params(0).t = Type.getBaseType(initT) // initial element type
        fReduce.params(1).t = Type.getBaseType(flatMapT) // reduce input array element type

        val reduceBodyType = TypeChecker.check(fReduce.body, setType) // check the body

        if (Type.getBaseType(initT) != Type.getBaseType(flatMapT) || Type.getBaseType(initT) != reduceBodyType)
          throw TypeException(
            s"Illegal reduction function in:\n``$this``.\n" +
              s"``(${Type.getBaseType(initT)}, ${Type.getBaseType(flatMapT)}) -> $reduceBodyType`` " +
              s"does not match ``(α, α) -> α``"
          )

//        ArrayTypeWSWC(initT, 1)
        initT

      case _ => throw new TypeException(argType, "TupleType(_, ArrayType(_, _))", this)
    }
  }

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = {
    fMap.visit_pp(prePost)
    fReduce.visit_pp(prePost)
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    throw new NotImplementedError()
  }
}

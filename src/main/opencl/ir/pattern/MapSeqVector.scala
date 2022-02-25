package opencl.ir.pattern

import ir.{ArrayType, ArrayTypeWSWC, Capacity, Memory, ScalarType, Size, Type, TypeChecker, TypeException, UnallocatedMemory, UndefType, VectorType}
import ir.ast.{FPattern, IRNode, Lambda, Lambda1, Pattern}
import ir.interpreter.Interpreter.ValueMap
import ir.view.View
import lift.arithmetic.{ArithExpr, Cst, PosVar, Var}

case class MapSeqVector(fVectorized: Lambda1,
                        fScalar: Lambda1,
                        vectorLen: ArithExpr) extends Pattern(arity = 1) {

  var vectorLoopVar: Var = PosVar("vi")
  var vectorCopyLoopVar: Var = PosVar("vi2")
  var scalarLoopVar: Var = PosVar("si")

  // In double options below, outer option = initialized / not initialized; inner option = whether there is vector/scalar component at all (depends on arg size)
  var argTVectorizedPart: Option[Option[ArrayType with Size with Capacity]] = None
  var argTScalarPart: Option[Option[ArrayType with Size with Capacity]] = None

  var outTVectorizedPart: Option[Option[ArrayType]] = None
  var outTScalarPart: Option[Option[ArrayType]] = None

  var fVectorizedOutputView: Option[Option[View]] = None

  var shouldUnroll = false

  def vectorPartNonEmpty: Boolean =
    outTVectorizedPart match {
      case Some(Some(_)) => true
      case Some(None) => false
      case None => throw new IllegalStateException("Cannot invoke vectorPartNonEmpty before the type checker is run")
    }

  def scalarPartNonEmpty: Boolean =
    outTScalarPart match {
      case Some(Some(_)) => true
      case Some(None) => false
      case None => throw new IllegalStateException("Cannot invoke vectorPartNonEmpty before the type checker is run")
    }

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case at @ ArrayTypeWSWC(et: ScalarType, s, c) =>
        assert(s == c)

        argTVectorizedPart = (s / vectorLen) match {
          case nVectors: ArithExpr
            if ArithExpr.isSmaller(nVectors, 1).getOrElse(false) =>
            Some(None)

          case nVectors: ArithExpr =>
            Some(Some(ArrayTypeWSWC(et.vectorize(vectorLen), nVectors)))
        }
        s % vectorLen match {
          case Cst(c) if c == 0 =>
            argTScalarPart = Some(None)
          case other =>
            argTScalarPart = Some(Some(ArrayTypeWSWC(et, other)))
        }

        fVectorized.params(0).t = et.vectorize(vectorLen)
        fScalar.params(0).t = et

        val fVectorizedT = TypeChecker.check(fVectorized.body, setType)
        val fScalarT = TypeChecker.check(fScalar.body, setType)

        fVectorizedT match {
          case VectorType(fVectorizedBaseT, _) if fVectorizedBaseT == fScalarT =>
          case _ => throw new TypeException(fVectorizedT, s"VectorType($fScalarT, $vectorLen)", fVectorized)
        }

        outTVectorizedPart = Some(argTVectorizedPart.get match {
          case Some(argATVectorPart) => Some(argATVectorPart.replacedElemT(fVectorizedT))
          case None => None
        })
        outTScalarPart = Some(argTScalarPart.get match {
          case Some(argATScalarPart) => Some(argATScalarPart.replacedElemT(fScalarT))
          case None => None
        })

        if (!vectorPartNonEmpty && !scalarPartNonEmpty)
          throw TypeException(s"With arg len of $s, both vector and scalar loops of MapSeqVector perform zero iterations")

        at.replacedElemT(fScalarT)

      case _ => throw new TypeException(argType, "ArrayType(ScalarType)", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    args.head match {
      case a: Vector[_] => a.map(fScalar.eval(valueMap, _))
    }
  }

  override def toString: String = "MapSeqVector(" + fVectorized + ", " + fScalar + ")"

  def copy(fVectorized: Lambda1, fScalar: Lambda1): Pattern =
    MapSeqVector(fVectorized, fScalar, vectorLen)

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = {
    fVectorized.visit_pp(prePost)
    fScalar.visit_pp(prePost)
  }

  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode =
    MapSeqVector(
      fVectorized.visitAndRebuild(pre,post).asInstanceOf[Lambda1],
      fScalar.visitAndRebuild(pre,post).asInstanceOf[Lambda1],
      vectorLen)
}

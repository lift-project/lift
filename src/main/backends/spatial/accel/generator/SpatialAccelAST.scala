package backends.spatial.accel.generator

import backends.spatial.common.SpatialAST.SpatialAddressSpaceOperator
import backends.spatial.common.ir.{RegMemory, SRAMMemory, SpatialAddressSpace, UndefAddressSpace}
import core.generator.GenericAST
import core.generator.GenericAST.{AddressorT, ArithExpression, AstNode, CVar, ExpressionT, MutableExprBlockT, StatementT, VarDeclT, Pipe}
import core.generator.PrettyPrinter.{Doc, empty, text, stringToDoc}
import ir.{ArrayType, Type}
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import utils.Printer

object SpatialAccelAST {

  /**
   * An index interval for array slice access
   */
  trait IdxIntervalT extends AddressorT with ExpressionT {
    val idxStart: ArithExpression
    val idxStop: ArithExpression

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      visitFun(z, this)
    }

    override def print(): Doc = idxStart.print <> "::" <> idxStop.print
  }

  case class IdxInterval(idxStart: ArithExpression,
                         idxStop: ArithExpression) extends IdxIntervalT {

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode =
      IdxInterval(
        idxStart.visitAndRebuild(pre, post).asInstanceOf[ArithExpression],
        idxStop.visitAndRebuild(pre, post).asInstanceOf[ArithExpression])

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      idxStart.visitBy(pre, post)
      idxStop.visitBy(pre, post)
    }
  }

  /**
   * A reference to a declared variable that supports multidimensional and sliced array reference
   */
  trait NDVarSlicedRefT extends ExpressionT {
    val v: CVar
    val suffix: Option[String]
    val arrayAddressors: Option[List[AddressorT]]

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      visitFun(z, this) |>
        (_z => arrayAddressors match {
          case Some(addressors) => addressors.foldLeft(_z) {
            case (acc, node) => node.visit(acc)(visitFun)
          }
          case None => _z
        }) |>
        (visitFun(_, v))
    }

    override def print(): Doc = {

      val accessD = arrayAddressors match {
        case None => empty
        case Some(singleAccessor :: Nil) => "[" <> singleAccessor.print <> "]"
        case Some(firstAccessor :: remainingAccessors) =>
          remainingAccessors.foldLeft("[" <> firstAccessor.print)(_ <> _.print) <> "]"
      }

      val suffixD = suffix match {
        case None     => empty
        case Some(sf) => text(sf)
      }

      v.print <> accessD <> suffixD
    }
  }

  case class NDVarSlicedRef(v: CVar,
                            suffix: Option[String] = None,
                            arrayAddressors: Option[List[AddressorT]] = None) extends NDVarSlicedRefT {

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      NDVarSlicedRef(
        v.visitAndRebuild(pre, post).asInstanceOf[CVar], suffix,
        arrayAddressors match {
          case Some(addressors) => Some(addressors.map(_.visitAndRebuild(pre, post).asInstanceOf[AddressorT]))
          case None => None
        })
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      arrayAddressors match {
        case Some(addressors) => addressors.foreach(_.visitBy(pre, post))
        case None =>
      }
      v.visitBy(pre, post)
    }
  }

  case class SpatialVarDecl(v: GenericAST.CVar,
                            t: Type,
                            init: Option[AstNode] = None,
                            addressSpace: SpatialAddressSpace = UndefAddressSpace)
  extends VarDeclT with SpatialAddressSpaceOperator {
    val length = 0 // Use multidimensional shape in the type instead

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      SpatialVarDecl(v.visitAndRebuild(pre, post).asInstanceOf[GenericAST.CVar], t,
        init match {
          case Some(i) => Some(i.visitAndRebuild(pre, post))
          case None => None
        },  addressSpace)
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      v.visitBy(pre, post)
      init match {
        case Some(i) => i.visitBy(pre, post)
        case None =>
      }
    }

    override def print(): Doc = t match {
      case _: ArrayType =>
        addressSpace match {
          case RegMemory => throw new NotImplementedException() // TODO: unroll Reg memory
          case SRAMMemory =>
            val baseType = Type.getBaseType(t)

            s"val ${Printer.toString(v.v)} = $addressSpace[${Printer.toString(baseType)}]" <>
              "(" <> Type.getLengths(t).map(Printer.toString).reduce(_ <> ", " <> _) <> ")"
          case _ => throw new NotImplementedError()
        }

      case _ =>
        val baseType = Type.getBaseType(t)

        s"$addressSpace[${Printer.toString(baseType)}]" <>
          ((addressSpace, init) match {
            case (RegMemory, Some(initNode)) => "(" <> initNode.print() <> ")"
            case (RegMemory,None) => empty
            case (SRAMMemory, _) => "(1)"
            case _ => throw new NotImplementedError() // TODO
          })
    }
  }

  trait CounterT extends ExpressionT {
    val min: ExpressionT
    val max: ExpressionT
    val stride: ExpressionT
    val factor: ExpressionT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (min.visit(_)(visitFun)) |>
        (max.visit(_)(visitFun)) |>
        (stride.visit(_)(visitFun)) |>
        (factor.visit(_)(visitFun))
    }

    override def print(): Doc = {
      min.print <> text("until") <> max.print <> text("by") <>
        stride.print <> text("par") <> factor.print
    }
  }

  case class Counter(min: ExpressionT,
                     max: ExpressionT,
                     stride: ExpressionT,
                     factor: ExpressionT) extends CounterT {
    def _visitAndRebuild(pre: (AstNode) => AstNode,  post: (AstNode) => AstNode) : AstNode = {
      Counter(min.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        max.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        stride.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        factor.visitAndRebuild(pre, post).asInstanceOf[ExpressionT])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      min.visitBy(pre, post)
      max.visitBy(pre, post)
      stride.visitBy(pre, post)
      factor.visitBy(pre, post)
    }
  }

  trait ReduceT extends StatementT {
    val accum: AstNode
    val counter: List[CounterT]
    val mapFun: MutableExprBlockT
    val reduceFun: MutableExprBlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        // Visit internal expressions of a for loop
        (accum.visit(_)(visitFun)) |>
        (counter.foldLeft(_) {
          case (acc, node) => node.visit(acc)(visitFun)
        }) |>
        (mapFun.visit(_)(visitFun)) |>
        (reduceFun.visit(_)(visitFun))
    }

    override def print(): Doc = {
      text("Reduce(") <> accum.print <> text(")") <>
        text("(") <> counter.map(_.print()).reduce(_ <> text(",") <> _) <> text(")") <>
        mapFun.print <> reduceFun.print
    }
  }

  case class Reduce(accum: AstNode,
                    counter: List[CounterT],
                    mapFun: MutableExprBlockT,
                    reduceFun: MutableExprBlockT) extends ReduceT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      Reduce(accum.visitAndRebuild(pre, post),
        counter.map(_.visitAndRebuild(pre, post).asInstanceOf[CounterT]),
        mapFun.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT],
        reduceFun.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {
      accum.visitBy(pre, post)
      counter.foreach(_.visitBy(pre, post))
      mapFun.visitBy(pre, post)
      reduceFun.visitBy(pre, post)
    }
  }
}

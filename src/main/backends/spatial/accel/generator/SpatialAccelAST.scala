package backends.spatial.accel.generator

import backends.spatial.common.SpatialAST.SpatialAddressSpaceOperator
import backends.spatial.common.ir.{RegMemory, SRAMMemory, SpatialAddressSpace, UndefAddressSpace}
import core.generator.GenericAST
import core.generator.GenericAST.{ArithExpression, ArithExpressionT, AstNode, CVar, ExpressionT, MutableExprBlockT, Pipe, StatementT, VarDeclT}
import core.generator.PrettyPrinter.{Doc, empty, stringToDoc, text}
import ir.{ArrayType, Type}
import lift.arithmetic.ArithExpr
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import utils.Printer

object SpatialAccelAST {

  /**
   * Array addressor such as an index or a slice
   */
  trait AddressorT extends ExpressionT

  /**
   * Indexed array access
   */
  trait ArrIndexT extends AddressorT with ArithExpressionT

  case class ArrIndex(override val content: ArithExpr) extends ArithExpression(content) with ArrIndexT

  /**
   * Sliced array access
   */
  trait ArrSliceT extends AddressorT with ExpressionT {
    val start: ArithExpression
    val step: ArithExpression
    val end: ArithExpression

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |> (visitFun(_, this)) |>
        (visitFun(_, start)) |>
        (visitFun(_, step)) |>
        (visitFun(_, step))
    }

    override def print(): Doc = start.print <> "::" <> end.print
  }

  case class ArrSlice(start: ArithExpression,
                      step: ArithExpression,
                      end: ArithExpression) extends ArrSliceT {

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode =
      ArrSlice(
        start.visitAndRebuild(pre, post).asInstanceOf[ArithExpression],
        step.visitAndRebuild(pre, post).asInstanceOf[ArithExpression],
        end.visitAndRebuild(pre, post).asInstanceOf[ArithExpression])

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      start.visitBy(pre, post)
      step.visitBy(pre, post)
      end.visitBy(pre, post)
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

  trait AbstractReduceT extends StatementT {
    val reduceFlavour: String
    val accum: AstNode
    val counter: List[CounterT]
    val mapBody: MutableExprBlockT
    val reduceBody: MutableExprBlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        // Visit internal expressions of the for loop
        (accum.visit(_)(visitFun)) |>
        (counter.foldLeft(_) {
          case (acc, node) => node.visit(acc)(visitFun)
        }) |>
        (mapBody.visit(_)(visitFun)) |>
        (reduceBody.visit(_)(visitFun))
    }

    override def print(): Doc = {
      reduceFlavour <> text("(") <> accum.print <> text(")") <>
        text("(") <> counter.map(_.print()).reduce(_ <> text(",") <> _) <> text(")") <>
        mapBody.print <> reduceBody.print
    }
  }

  case class Reduce(accum: AstNode,
                    counter: List[CounterT],
                    mapBody: MutableExprBlockT,
                    reduceBody: MutableExprBlockT) extends AbstractReduceT {
    val reduceFlavour: String = "Reduce"
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      Reduce(accum.visitAndRebuild(pre, post),
        counter.map(_.visitAndRebuild(pre, post).asInstanceOf[CounterT]),
        mapBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT],
        reduceBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {
      accum.visitBy(pre, post)
      counter.foreach(_.visitBy(pre, post))
      mapBody.visitBy(pre, post)
      reduceBody.visitBy(pre, post)
    }
  }

  case class Fold(accum: AstNode,
                  counter: List[CounterT],
                  mapBody: MutableExprBlockT,
                  reduceBody: MutableExprBlockT) extends AbstractReduceT {
    val reduceFlavour: String = "Fold"
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      Fold(accum.visitAndRebuild(pre, post),
        counter.map(_.visitAndRebuild(pre, post).asInstanceOf[CounterT]),
        mapBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT],
        reduceBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {
      accum.visitBy(pre, post)
      counter.foreach(_.visitBy(pre, post))
      mapBody.visitBy(pre, post)
      reduceBody.visitBy(pre, post)
    }
  }

  case class MemFold(accum: AstNode,
                     counter: List[CounterT],
                     mapBody: MutableExprBlockT,
                     reduceBody: MutableExprBlockT) extends AbstractReduceT {
    val reduceFlavour: String = "MemFold"
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      MemFold(accum.visitAndRebuild(pre, post),
        counter.map(_.visitAndRebuild(pre, post).asInstanceOf[CounterT]),
        mapBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT],
        reduceBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {
      accum.visitBy(pre, post)
      counter.foreach(_.visitBy(pre, post))
      mapBody.visitBy(pre, post)
      reduceBody.visitBy(pre, post)
    }
  }



  trait ForeachT extends StatementT {
    val counter: List[CounterT]
    val body: MutableExprBlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        // Visit internal expressions of the for loop
        (counter.foldLeft(_) {
          case (acc, node) => node.visit(acc)(visitFun)
        }) |>
        (body.visit(_)(visitFun))
    }

    override def print(): Doc = {
      text("Foreach") <>
        text("(") <> counter.map(_.print()).reduce(_ <> text(",") <> _) <> text(")") <>
        body.print
    }
  }

  case class Foreach(counter: List[CounterT],
                     body: MutableExprBlockT) extends ForeachT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      Foreach(
        counter.map(_.visitAndRebuild(pre, post).asInstanceOf[CounterT]),
        body.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {
      counter.foreach(_.visitBy(pre, post))
      body.visitBy(pre, post)
    }
  }
}

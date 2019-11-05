package backends.spatial.accel.ir.ast

import backends.spatial.common.SpatialAST.SpatialAddressSpaceOperator
import backends.spatial.common.ir.{LiteralMemory, RegMemory, SRAMMemory, SpatialAddressSpace, UndefAddressSpace}
import core.generator.GenericAST
import core.generator.GenericAST._
import core.generator.PrettyPrinter._
import ir.{ArrayTypeWS, Type}
import lift.arithmetic.ArithExpr
import backends.spatial.common.Printer

object SpatialAccelAST {
  /**
   * Determines whether to generate dot or infix notation in Scala.
   * In dot notation, method calls are expressed as "this.f(that)".
   * For example, the plus operation would be expressed as "this.+(that)".
   * In infix (prefix) notation, method calls with two or one arguments are
   * expressed as "this f that" or "this f" respectively.
   * At this point, the difference is cosmetic.
   */
  val infixNotation: Boolean = true

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
  trait VarSlicedRefT extends VarRefT {
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
        case Some(Nil) => throw new IllegalArgumentException("Expected at least one array addressor. Got none")
        case Some(singleAccessor :: Nil) => "(" <> singleAccessor.print <> ")"
        case Some(firstAccessor :: remainingAccessors) =>
          remainingAccessors.foldLeft("(" <> firstAccessor.print)(_ <> ", " <> _.print) <> ")"
      }

      val suffixD = suffix match {
        case None     => empty
        case Some(sf) => text(sf)
      }

      v.print <> accessD <> suffixD
    }
  }

  case class VarSlicedRef(v: CVar,
                          suffix: Option[String] = None,
                          arrayAddressors: Option[List[AddressorT]] = None) extends VarSlicedRefT {

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      VarSlicedRef(
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
    val length: Long = 0 // Use multidimensional shape in the type instead

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

    override def print(): Doc = {
        (t match {
          case ArrayTypeWS(_, size) =>
            addressSpace match {
              case SRAMMemory =>
                val baseType = Type.getBaseType(t)
                val bufferDimensions = Type.getLengths(t).dropRight(1) // Remove the extra dimension for the scalar base type

                "val" <+> Printer.toString(v.v) <+> "=" <+>
                  s"${Printer.toString(addressSpace)}[${Printer.toString(baseType)}]" <>
                  "(" <> bufferDimensions.map(Printer.toString).map(text).reduce(_ <> ", " <> _) <> ")"
              case RegMemory =>
                // Unroll register memory
                stack(List.tabulate(size.evalInt)(i => {
                  "val" <+> Printer.toString(v.v) <> "_" <> Printer.toString(i)  <+> "=" <+>
                    s"${Printer.toString(addressSpace)}[${Printer.toString(Type.getValueType(t))}]" }))

              case LiteralMemory => throw new IllegalArgumentException("Cannot print literal variable declaration")
              case _ => throw new NotImplementedError()
            }

          case _ =>
            val baseType = Type.getBaseType(t)

            "val" <+> Printer.toString(v.v) <+> "=" <+>
              s"$addressSpace[${Printer.toString(baseType)}]" <>
              ((addressSpace, init) match {
                case (RegMemory, Some(initNode)) => "(" <> initNode.print() <> ")"
                case (RegMemory, None) => empty
                case (SRAMMemory, _) => "(1)"
                case (LiteralMemory, _) => throw new IllegalArgumentException("Cannot print literal variable declaration")
                case _ => throw new NotImplementedError() // TODO
              })
        })
    }
  }

  case class RegAssignmentExpression(to: AstNode, value: AstNode) extends
    AssignmentExpressionT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      RegAssignmentExpression(to.visitAndRebuild(pre, post),
        value.visitAndRebuild(pre, post))
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      to.visitBy(pre, post)
      value.visitBy(pre,post)
    }

    override def print(): Doc = {
      to.print <+> ":=" <+> value.print
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
      min.print <+> text("until") <+> max.print <+> text("by") <+>
        stride.print <+> text("par") <+> factor.print
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
    val iterVars: List[GenericAST.CVar]
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
        (iterVars.foldLeft(_) {
          case (acc, node) => node.visit(acc)(visitFun)
        }) |>
        (mapBody.visit(_)(visitFun)) |>
        (reduceBody.visit(_)(visitFun))
    }

    override def print(): Doc = {
      reduceFlavour <> text("(") <> accum.print <> text(")") <>
        text("(") <> counter.map(_.print()).reduce(_ <> text(",") <> _) <> text(")") <+>
        text("{") <+> text("(") <> intersperse(iterVars.map(_.print()), ", ") <> text(")") <+> text("=>") <>
        nest(2, line <> mapBody.print()) <> line <> "}" <+> "{" <>
        nest(2, line <> reduceBody.print()) <> line <> "}"
    }
  }

  case class Reduce(accum: AstNode,
                    counter: List[CounterT],
                    iterVars: List[GenericAST.CVar],
                    mapBody: MutableExprBlockT,
                    reduceBody: MutableExprBlockT) extends AbstractReduceT {
    val reduceFlavour: String = "Reduce"
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      Reduce(accum.visitAndRebuild(pre, post),
        counter.map(_.visitAndRebuild(pre, post).asInstanceOf[CounterT]),
        iterVars.map(_.visitAndRebuild(pre, post).asInstanceOf[GenericAST.CVar]),
        mapBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT],
        reduceBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {
      accum.visitBy(pre, post)
      counter.foreach(_.visitBy(pre, post))
      iterVars.foreach(_.visitBy(pre, post))
      mapBody.visitBy(pre, post)
      reduceBody.visitBy(pre, post)
    }
  }

  case class Fold(accum: AstNode,
                  counter: List[CounterT],
                  iterVars: List[GenericAST.CVar],
                  mapBody: MutableExprBlockT,
                  reduceBody: MutableExprBlockT) extends AbstractReduceT {
    val reduceFlavour: String = "Fold"
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      Fold(accum.visitAndRebuild(pre, post),
        counter.map(_.visitAndRebuild(pre, post).asInstanceOf[CounterT]),
        iterVars.map(_.visitAndRebuild(pre, post).asInstanceOf[GenericAST.CVar]),
        mapBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT],
        reduceBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {
      accum.visitBy(pre, post)
      counter.foreach(_.visitBy(pre, post))
      iterVars.foreach(_.visitBy(pre, post))
      mapBody.visitBy(pre, post)
      reduceBody.visitBy(pre, post)
    }
  }

  case class MemFold(accum: AstNode,
                     counter: List[CounterT],
                     iterVars: List[GenericAST.CVar],
                     mapBody: MutableExprBlockT,
                     reduceBody: MutableExprBlockT) extends AbstractReduceT {
    val reduceFlavour: String = "MemFold"
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      MemFold(accum.visitAndRebuild(pre, post),
        counter.map(_.visitAndRebuild(pre, post).asInstanceOf[CounterT]),
        iterVars.map(_.visitAndRebuild(pre, post).asInstanceOf[GenericAST.CVar]),
        mapBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT],
        reduceBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {
      accum.visitBy(pre, post)
      counter.foreach(_.visitBy(pre, post))
      iterVars.foreach(_.visitBy(pre, post))
      mapBody.visitBy(pre, post)
      reduceBody.visitBy(pre, post)
    }
  }



  trait ForeachT extends StatementT {
    val counter: List[CounterT]
    val iterVars: List[GenericAST.CVar]
    val body: MutableExprBlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        // Visit internal expressions of the for loop
        (counter.foldLeft(_) {
          case (acc, node) => node.visit(acc)(visitFun)
        }) |>
        (iterVars.foldLeft(_) {
          case (acc, node) => node.visit(acc)(visitFun)
        }) |>
        (body.visit(_)(visitFun))
    }

    override def print(): Doc = {
      text("Foreach") <>
        text("(") <> counter.map(_.print()).reduce(_ <> text(",") <> _) <> text(")") <+>
        text("{") <+> text("(") <> intersperse(iterVars.map(_.print()), ", ") <> text(")") <+> text("=>") <>
        nest(2, line <> body.print()) <> line <> "}"
    }
  }

  case class Foreach(counter: List[CounterT],
                     iterVars: List[GenericAST.CVar],
                     body: MutableExprBlockT) extends ForeachT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      Foreach(
        counter.map(_.visitAndRebuild(pre, post).asInstanceOf[CounterT]),
        iterVars.map(_.visitAndRebuild(pre, post).asInstanceOf[GenericAST.CVar]),
        body.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {
      counter.foreach(_.visitBy(pre, post))
      iterVars.foreach(_.visitBy(pre, post))
      body.visitBy(pre, post)
    }
  }

  /**
   * A load from a DRAM variable into a local variable
   */
  trait SpLoadT extends StatementT {
    val src: AstNode
    val target: VarSlicedRef

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, src)) |>
        (visitFun(_, target))
    }

    override def print(): Doc = {
      target.print() <> (if (infixNotation) " " else ".") <>
        "load" <> (if (infixNotation) " " else ".(") <>
        src.print() <> (if (infixNotation) "" else ")")
    }
  }

  case class SpLoad(src: AstNode,
                    target: VarSlicedRef) extends SpLoadT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      SpLoad(src.visitAndRebuild(pre, post), target.visitAndRebuild(pre, post).asInstanceOf[VarSlicedRef])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      src.visitBy(pre, post)
      target.visitBy(pre, post)
    }
  }

  /**
   * A store from an SRAM variable into DRAM variable
   */
  trait SpStoreT extends StatementT {
    val src: AstNode
    val target: VarSlicedRef

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, src)) |>
        (visitFun(_, target))
    }

    override def print(): Doc = {
      target.print() <> (if (infixNotation) " " else ".") <>
        "store" <> (if (infixNotation) " " else ".(") <>
        src.print() <> (if (infixNotation) "" else ")")
    }
  }

  case class SpStore(src: AstNode,
                     target: VarSlicedRef) extends SpStoreT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      SpStore(src.visitAndRebuild(pre, post), target.visitAndRebuild(pre, post).asInstanceOf[VarSlicedRef])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      src.visitBy(pre, post)
      target.visitBy(pre, post)
    }
  }
}

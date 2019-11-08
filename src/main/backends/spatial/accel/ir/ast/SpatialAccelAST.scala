package backends.spatial.accel.ir.ast

import backends.spatial.common.SpatialAST.SpatialAddressSpaceOperator
import backends.spatial.common.ir.{ArgOutMemory, DRAMMemory, LiteralMemory, RegMemory, SRAMMemory, SpatialAddressSpace, UndefAddressSpace}
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
   * Schedules. These AST nodes are used both for controller tags and scheduling control structures.
   * */
  trait ScheduleT extends AttributeT

  trait SequentialScheduleT extends ScheduleT {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |> (visitFun(_, this))
    }
    override def print(): Doc = "Sequential"
  }

  case class SequentialSchedule() extends SequentialScheduleT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = SequentialSchedule()
    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}
  }

  trait PipeScheduleT extends ScheduleT {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |> (visitFun(_, this))
    }
    override def print(): Doc = "Pipe"
  }

  case class PipeSchedule() extends PipeScheduleT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = PipeSchedule()
    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}
  }

  trait ParallelScheduleT extends ScheduleT {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |> (visitFun(_, this))
    }
    override def print(): Doc = "Parallel"
  }

  case class ParallelSchedule() extends ParallelScheduleT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = ParallelSchedule()
    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}
  }

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
    val end: ArithExpression

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |> (visitFun(_, this)) |>
        (visitFun(_, start)) |>
        (visitFun(_, end))
    }

    override def print(): Doc = start.print <> "::" <> end.print
  }

  case class ArrSlice(start: ArithExpression,
                      end: ArithExpression) extends ArrSliceT {

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode =
      ArrSlice(
        start.visitAndRebuild(pre, post).asInstanceOf[ArithExpression],
        end.visitAndRebuild(pre, post).asInstanceOf[ArithExpression])

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      start.visitBy(pre, post)
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
                            addressSpace: SpatialAddressSpace = UndefAddressSpace,
                            bufferHazard: Boolean = false)
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
      val decl = (t match {
        case ArrayTypeWS(_, size) =>
          "val" <+> Printer.toString(v.v) <>
            (addressSpace match {
              case SRAMMemory =>
                val baseType = Type.getBaseType(t)
                val bufferDimensions = Type.getLengths(t).dropRight(1) // Remove the extra dimension for the scalar base type

                " =" <+> s"${Printer.toString(addressSpace)}[${Printer.toString(baseType)}]" <>
                  "(" <> bufferDimensions.map(Printer.toString).map(text).reduce(_ <> ", " <> _) <> ")"
              case RegMemory =>
                // Unroll register memory
                stack(List.tabulate(size.evalInt)(i => {
                  "_" <> Printer.toString(i)  <+> "=" <+>
                    s"${Printer.toString(addressSpace)}[${Printer.toString(Type.getValueType(t))}]" }))

              case ArgOutMemory =>
                throw new IllegalArgumentException("Cannot print ArgOut variable declaration inside an Accel block")
              case LiteralMemory =>
                throw new IllegalArgumentException("Cannot print literal variable declaration")
              case _ => throw new NotImplementedError()
            })


        case _ =>
          val baseType = Type.getBaseType(t)

          "val" <+> Printer.toString(v.v) <+> "=" <+>
            s"$addressSpace[${Printer.toString(baseType)}]" <>
            ((addressSpace, init) match {
              case (RegMemory, Some(initNode)) => "(" <> initNode.print() <> ")"
              case (RegMemory, None) => empty
              case (SRAMMemory, _) => throw new IllegalArgumentException("Cannot declare a scalar as SRAM memory")
              case (DRAMMemory, _) => throw new IllegalArgumentException("Cannot declare a scalar as DRAM memory")
              case (ArgOutMemory, _) => throw new IllegalArgumentException(
                                          "Cannot print ArgOut variable declaration inside an Accel block")
              case (LiteralMemory, _) => throw new IllegalArgumentException("Cannot declare a literal as a variable")
              case _ => throw new NotImplementedError() // TODO
            })
      })

      if (bufferHazard) decl <> ".buffer" else decl
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
    val factor: Option[ExpressionT]

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      val result = z |>
        (visitFun(_, this)) |>
        (min.visit(_)(visitFun)) |>
        (max.visit(_)(visitFun)) |>
        (stride.visit(_)(visitFun))
      if (factor.isDefined) result |> (factor.get.visit(_)(visitFun))
      else result
    }

    override def print(): Doc = {
      val result = min.print <+> text("until") <+> max.print <+> text("by") <+>
        stride.print
      if (factor.isDefined) result <+> text("par") <+> factor.get.print
      else result
    }
  }

  case class Counter(min: ExpressionT,
                     max: ExpressionT,
                     stride: ExpressionT,
                     factor: Option[ExpressionT]) extends CounterT {
    def _visitAndRebuild(pre: (AstNode) => AstNode,  post: (AstNode) => AstNode) : AstNode = {
      Counter(min.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        max.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        stride.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        if (factor.isDefined) Some(factor.get.visitAndRebuild(pre, post).asInstanceOf[ExpressionT])
        else factor)
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      min.visitBy(pre, post)
      max.visitBy(pre, post)
      stride.visitBy(pre, post)
      if (factor.isDefined) factor.get.visitBy(pre, post)
    }
  }

  trait AbstractReduceT extends StatementT { // TODO: consider making it ExpressionT as well
    val scheduleDirective: ScheduleT
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
        (scheduleDirective.visit(_)(visitFun)) |>
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
      scheduleDirective.print() <> text(".") <> reduceFlavour <> text("(") <> accum.print <> text(")") <>
        text("(") <> counter.map(_.print()).reduce(_ <> text(",") <> _) <> text(")") <+>
        text("{") <+> text("(") <> intersperse(iterVars.map(_.print()), ", ") <> text(")") <+> text("=>") <>
        nest(2, line <> mapBody.print()) <> line <> "}" <+> "{" <>
        nest(2, line <> reduceBody.print()) <> line <> "}"
    }
  }

  case class Reduce(scheduleDirective: ScheduleT,
                    accum: AstNode,
                    counter: List[CounterT],
                    iterVars: List[GenericAST.CVar],
                    mapBody: MutableExprBlockT,
                    reduceBody: MutableExprBlockT) extends AbstractReduceT {
    val reduceFlavour: String = "Reduce"
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      Reduce(scheduleDirective.visitAndRebuild(pre, post).asInstanceOf[ScheduleT],
        accum.visitAndRebuild(pre, post),
        counter.map(_.visitAndRebuild(pre, post).asInstanceOf[CounterT]),
        iterVars.map(_.visitAndRebuild(pre, post).asInstanceOf[GenericAST.CVar]),
        mapBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT],
        reduceBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {
      scheduleDirective.visitBy(pre, post)
      accum.visitBy(pre, post)
      counter.foreach(_.visitBy(pre, post))
      iterVars.foreach(_.visitBy(pre, post))
      mapBody.visitBy(pre, post)
      reduceBody.visitBy(pre, post)
    }
  }

  case class Fold(scheduleDirective: ScheduleT,
                  accum: AstNode,
                  counter: List[CounterT],
                  iterVars: List[GenericAST.CVar],
                  mapBody: MutableExprBlockT,
                  reduceBody: MutableExprBlockT) extends AbstractReduceT {
    val reduceFlavour: String = "Fold"
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      Fold(scheduleDirective.visitAndRebuild(pre, post).asInstanceOf[ScheduleT],
        accum.visitAndRebuild(pre, post),
        counter.map(_.visitAndRebuild(pre, post).asInstanceOf[CounterT]),
        iterVars.map(_.visitAndRebuild(pre, post).asInstanceOf[GenericAST.CVar]),
        mapBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT],
        reduceBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {
      scheduleDirective.visitBy(pre, post)
      accum.visitBy(pre, post)
      counter.foreach(_.visitBy(pre, post))
      iterVars.foreach(_.visitBy(pre, post))
      mapBody.visitBy(pre, post)
      reduceBody.visitBy(pre, post)
    }
  }

  case class MemFold(scheduleDirective: ScheduleT,
                     accum: AstNode,
                     counter: List[CounterT],
                     iterVars: List[GenericAST.CVar],
                     mapBody: MutableExprBlockT,
                     reduceBody: MutableExprBlockT) extends AbstractReduceT {
    val reduceFlavour: String = "MemFold"
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      MemFold(scheduleDirective.visitAndRebuild(pre, post).asInstanceOf[ScheduleT],
        accum.visitAndRebuild(pre, post),
        counter.map(_.visitAndRebuild(pre, post).asInstanceOf[CounterT]),
        iterVars.map(_.visitAndRebuild(pre, post).asInstanceOf[GenericAST.CVar]),
        mapBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT],
        reduceBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {
      scheduleDirective.visitBy(pre, post)
      accum.visitBy(pre, post)
      counter.foreach(_.visitBy(pre, post))
      iterVars.foreach(_.visitBy(pre, post))
      mapBody.visitBy(pre, post)
      reduceBody.visitBy(pre, post)
    }
  }



  trait ForeachT extends StatementT {
    val scheduleDirective: ScheduleT
    val counter: List[CounterT]
    val iterVars: List[GenericAST.CVar]
    val body: MutableExprBlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        // Visit internal expressions of the for loop
        (scheduleDirective.visit(_)(visitFun)) |>
        (counter.foldLeft(_) {
          case (acc, node) => node.visit(acc)(visitFun)
        }) |>
        (iterVars.foldLeft(_) {
          case (acc, node) => node.visit(acc)(visitFun)
        }) |>
        (body.visit(_)(visitFun))
    }

    override def print(): Doc = {
      scheduleDirective.print() <> text(".") <> text("Foreach") <>
        text("(") <> counter.map(_.print()).reduce(_ <> text(",") <> _) <> text(")") <+>
        text("{") <+> text("(") <> intersperse(iterVars.map(_.print()), ", ") <> text(")") <+> text("=>") <>
        nest(2, line <> body.print()) <> line <> "}"
    }
  }

  case class Foreach(scheduleDirective: ScheduleT,
                     counter: List[CounterT],
                     iterVars: List[GenericAST.CVar],
                     body: MutableExprBlockT) extends ForeachT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      Foreach(scheduleDirective.visitAndRebuild(pre, post).asInstanceOf[ScheduleT],
        counter.map(_.visitAndRebuild(pre, post).asInstanceOf[CounterT]),
        iterVars.map(_.visitAndRebuild(pre, post).asInstanceOf[GenericAST.CVar]),
        body.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {
      scheduleDirective.visitBy(pre, post)
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
    val target: AstNode

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
                     target: AstNode) extends SpStoreT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      SpStore(src.visitAndRebuild(pre, post), target.visitAndRebuild(pre, post))
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      src.visitBy(pre, post)
      target.visitBy(pre, post)
    }
  }

  /**
   * A control structure defining a schedule
   */
  trait ScheduleControllerT extends StatementT { // TODO: consider making it ExpressionT as well
    val schedule: ScheduleT
    val body: MutableExprBlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (schedule.visit(_)(visitFun)) |>
        (body.visit(_)(visitFun))
    }

    override def print(): Doc = {
      schedule.print() <+> "{" <> nest(2, line <> body.print()) <> line <> "}"
    }
  }

  case class ScheduleController(schedule: ScheduleT,
                                body: MutableExprBlockT) extends ScheduleControllerT {

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      ScheduleController(
        schedule.visitAndRebuild(pre, post).asInstanceOf[ScheduleT],
        body.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {
      schedule.visitBy(pre, post)
      body.visitBy(pre, post)
    }

  }
}

package ir.ast

import ir.interpreter.Interpreter.ValueMap
import ir.view.{NoView, View}
import ir.{ArrayType, Memory, NoType, Size, TupleType, Type}
import lift.arithmetic.ArithExpr
import opencl.ir.pattern.ReduceSeq
import opencl.ir.{OpenCLMemory, OpenCLMemoryCollection}
import rewriting.passes.RewritingPass.globalNumbering
//import rewriting.passes.RewritingPass.globalNumbering

object CurrentColor {
  val colors: Array[String] = Array( "red", "green", "yellow", "1", "2", "3")
  var index : Int = 0
  def getCurrentColor() : String = colors(index)
  def increaseColorIndex() : Unit = { index = index + 1; assert(index < colors.length) }
}

/** Function calls, ie.: map(f, x), zip(x, y), ...
  *
  * Refers back to the function decl (e.g. map(f)) and the arguments (e.g. x)
  */
case class FunCall(f: FunDecl, args: Expr*) extends Expr with Cloneable {
  assert(f != null)

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = {
    args.map(_.visit_pp(prePost))
    f.visit_pp(prePost)
  }

  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = {
    val new_fc = FunCall(f.visitAndRebuild(pre, post).asInstanceOf[FunDecl], args.map(_.visitAndRebuild(pre, post).asInstanceOf[Expr]): _*)
    new_fc.t = this.t
    new_fc.gid = this.gid
    new_fc
  }

  def vectorize(vecSize: ArithExpr) : FunCall = {
    FunCall(VectorizeFunCall(vecSize), this )
  }


  def toStringForDebugging = {
    val fS = {
      val fStr = f.toString

      (f, this.t) match {
        case (m: AbstractMap, at: ArrayType with Size) =>
          fStr.replaceFirst(m.name, s"${m.name}.${at.size}.")
        case (_: ReduceSeq, _: ArrayType with Size) if args(1).t.isInstanceOf[ArrayType with Size] =>
          fStr.replaceFirst("ReduceSeq", s"ReduceSeq.${args(1).t.asInstanceOf[ArrayType with Size].size}.")
        case _ => fStr
      }
    }

    (if (globalNumbering != null)
      globalNumbering.get(this) match {
        case Some(n) => s"$n."
        case None => ""
      } else "") +
      (this match {
      case FunCall(Reduce(_), init, argCall@FunCall(_, _*)) =>
        //this.t + " " +
        fS + s"($init) o " + argCall
      // For debugging purposes
      case FunCall(_: UserFun, argCall@FunCall(_, _*)) =>
        //this.t + " " +
        fS + "." + this.mem.variable + " o " + argCall + "." + argCall.mem.variable
      case FunCall(_, argCall@FunCall(_, _*)) =>
        //this.t + " " +
//        (if (this.t.isInstanceOf[ArrayType]) f"${this.t.asInstanceOf[ArrayType with Size].size}." else "") +
        fS + " o " + argCall
      // For debugging purposes
      case FunCall(_: UserFun, args @ _*) =>
        //this.t + " " +
        fS + "." + this.mem.variable + " $ " +
          ( args.length match {
            case 1 => args.map(arg => arg + "." + arg.mem.variable).mkString("")
            case _ => "(" + args.map(arg => arg + "." + arg.mem.variable).mkString(", ") + ")"
          } )
      case _ =>
        //this.t + " " +
          fS + " $ " +
          ( args.length match {
        case 1 => args.mkString("")
        case _ => "(" + args.mkString(", ") + ")"
      } )
    })
  }


  override def toString = /*toStringForDebugging*/{
    val fS = f.toString

    (this match {
      case FunCall(Reduce(_), init, argCall@FunCall(_, _*)) =>
        fS + s"($init) o " + argCall

      case FunCall(_, argCall@FunCall(_, _*)) =>
        fS + " o " + argCall

      case FunCall(_: UserFun, args @ _*) =>
        fS + " $ " +
          ( args.length match {
            case 1 => args.map(arg => arg).mkString("")
            case _ => "(" + args.map(arg => arg).mkString(", ") + ")"
          } )
      case _ =>
        fS + " $ " +
          ( args.length match {
            case 1 => args.mkString("")
            case _ => "(" + args.mkString(", ") + ")"
          } )
    })
  }

  override def copy: FunCall = {
    this.clone().asInstanceOf[FunCall]
  }

  /**
   * One type for all arguments (i.e. a tuple if there are more than one args
   * */
  def argsType: Type = {
    if (args.length == 1) args(0).t
    else TupleType(args.map(_.t): _*)
  }

  def argsMemory: Memory = {
    if (args.length == 1) args(0).mem
    else OpenCLMemoryCollection(args.map(_.mem.asInstanceOf[OpenCLMemory]))
  }

  override def eval(valueMap: ValueMap): Any = {
    val argValues = args.map(_.eval(valueMap))
    f match {
      case l: Lambda  => l.eval(valueMap, argValues:_*)

      case p: Pattern => p.eval(valueMap, argValues:_*)

      case uf: UserFun => uf.eval(valueMap, argValues:_*)
    }
  }
}

object FunCallInst {
  def unapply(l: Lambda): Option[(Pattern,Expr)] = l match {
    case Lambda(_, FunCall(x, a), _) if x.isInstanceOf[Pattern] => Some((x.asInstanceOf[Pattern], a))
    case _ => None
  }
}

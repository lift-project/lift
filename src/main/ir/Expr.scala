package ir

import arithmetic.{Var, ?, ArithExpr, RangeUnknown}
import ir.view.{NoView, View}
import opencl.ir._

import language.implicitConversions

abstract class ExprVisitor {
  def accept(expr: Expr)
}

/** Base class for all expressions, ie.
  *
  * - function calls: map(f, x), zip(x,y), ...
  *
  * - parameter: x, y, ...
  *
  * - values: 4, 128, ...
  */
sealed abstract class Expr {
  var context: Context = null

  // type information
  var t: Type = UndefType

  // memory information
  var mem: Memory = UnallocatedMemory

  // view explaining how to access the memory
  var view: View = NoView

  var inputDepth: List[(ArithExpr, ArithExpr)] = List()
  var outputDepth: List[(ArithExpr, ArithExpr)] = List()

  def setContext(ctx: Context): Expr = {
    if (ctx != null)
      this.context = ctx
    this
  }

  def isConcrete: Boolean = {
    Expr.visit(false)(this, (e: Expr, b: Boolean) => {
      e match {
        case call: FunCall =>
          call.f match {
            case _: UserFunDef => true
            case _ => b
          }
        case _ => b
      }
    })
  }

  def isAbstract: Boolean = !isConcrete

  def addressSpace: OpenCLAddressSpace = OpenCLMemory.asOpenCLMemory(this.mem).addressSpace

  def containsLocal: Boolean = {
    containsMemory(LocalMemory)
  }

  private def containsMemory(memType: OpenCLAddressSpace): Boolean = {
    this.mem match {
      case coll: OpenCLMemoryCollection => coll.subMemories.exists(x => x.addressSpace == memType)
      case m: OpenCLMemory => m.addressSpace == memType
      case _ => false
    }
  }

  def containsPrivate: Boolean = {
    containsMemory(PrivateMemory)
  }

  def visit(visitor: ExprVisitor) {
    visitor.accept(this)
    this match {
      case call: FunCall =>
        call.args.foreach(_.visit(visitor))

        call.f match {
          case fp: FPattern => fp.f.body.visit(visitor)
          case cf: CompFunDef => cf.funs.foreach(_.body.visit(visitor))
          case l: Lambda => l.body.visit(visitor)
          case f: UserFunDef =>
        }
      case p:Param =>
    }
  }


  def copy: Expr
}

object Expr {

  def replace(e: Expr, oldE: Expr, newE: Expr): Expr =
    visit(e, (e: Expr) => if (e.eq(oldE)) newE else oldE, (e: Expr) => e)


  def visit[T](z: T)(expr: Expr, visitFun: (Expr, T) => T): T = {
    val result = visitFun(expr, z)
    expr match {
      case call: FunCall =>
        // visit args first
        val newResult = call.args.foldRight(result)((arg, x) => visit(x)(arg, visitFun))

        // do the rest ...
        call.f match {
          case fp: FPattern => visit(newResult)(fp.f.body, visitFun)
          case cf: CompFunDef => cf.funs.foldRight(newResult)((inF, x) => visit(x)(inF.body, visitFun))
          case l: Lambda => visit(newResult)(l.body, visitFun)
          case _ => newResult
        }
      case _ => result
    }
  }

  /**
   * Visit the expression of function f (recursively) and rebuild along the way
   */
  def visitArithExpr(expr: Expr, exprF: (ArithExpr) => (ArithExpr)): Expr = {
    visit(expr, {
      case call: FunCall =>
        call.f match {
          case Split(e) => Split(exprF(e))(call.args: _*)
          case asVector(e) => asVector(exprF(e))(call.args: _*)
          case _ => call
        }
      case inExpr => inExpr
    },
    (inExpr: Expr) => inExpr)
  }

  /**
   * Visit the function f recursively and rebuild along the way
   */
  def visit(expr: Expr, pre: (Expr) => (Expr), post: (Expr) => (Expr)): Expr = {
    var newExpr = pre(expr)
    newExpr = newExpr match {
      case call: FunCall =>
        val newArgs = call.args.map((arg) => visit(arg, pre, post))
        call.f match {
          case cf: CompFunDef =>
            CompFunDef(cf.funs.map(inF => new Lambda(inF.params, visit(inF.body, pre, post))): _*).apply(newArgs: _*)

          case ar: AbstractPartRed =>
            ar.getClass.getConstructor(classOf[Lambda], classOf[Value])
              .newInstance(visit(ar.f.body, pre, post), ar.init).apply(newArgs: _*)

          case fp: FPattern =>
            fp.getClass.getConstructor(classOf[Expr])
              .newInstance(visit(fp.f.body, pre, post)).apply(newArgs: _*)

          case _ => newExpr.copy
        }
      case _ => newExpr.copy
    }
    post(newExpr)
  }

  /**
   * Visit the function f recursively
   */
  def visit(expr: Expr, pre: (Expr) => (Unit), post: (Expr) => (Unit)): Unit = {
    pre(expr)
    expr match {
      case call: FunCall =>
        call.args.foreach((arg) => visit(arg, pre, post))

        call.f match {
          case fp: FPattern => visit(fp.f.body, pre, post)
          case l: Lambda => visit(l.body, pre, post)
          case cf: CompFunDef => cf.funs.reverseMap(inF => visit(inF.body, pre, post))
          case _ =>
        }
      case _ =>
    }
    post(expr)
  }

}


/** Parameters to functions and lambdas, i.e.: x, y, ...
  */
class Param() extends Expr with Cloneable {
  t = UndefType

  /**
   * Vectorize the current parameter
   * @param n The vector width
   * @return A vectorized parameter
   */
  def vectorize(n: ArithExpr): Param = this match {
    case v:VectorParam => throw new TypeException("Cannot vectorize a vectorized parameter")
    case x => new VectorParam(x, n)
  }

  override def toString = "PARAM"

  override def copy: Param = this.clone().asInstanceOf[Param]
}

object Param {
  def apply(): Param = new Param

  def apply(outT: Type): Param = {
    val p = Param()
    p.t = outT
    p
  }

  @deprecated("used Param.vectorize(n)")
  def vectorize(p: Param, n: ArithExpr): Param = p.vectorize(n)
}

/** A vectorized parameter*/
class VectorParam(val p: Param, n: ArithExpr) extends Param {
  t = p.t.vectorize(n)
}

/** A reference to a parameter wrapped in a tupel possibly produced by a zip.*/
class ParamReference(val p: Param, val i: Int) extends Param {
  override def toString = "PARAM REF"
}

/** Shortcut to access parameter in a tuple, i.e., fun( p => Get(p, 0) ) $ Zip(x, y) == x*/
object Get {
  def apply(p: Param, i: Int): ParamReference = new ParamReference(p, i)
}


/** Values, i.e.: 4, 128, ...*/
case class Value(var value: String) extends Param {
  override def copy: Value = this.clone().asInstanceOf[Value]

  override def toString = "VALUE"

  /**
   * Vectorize the current value.
   * @param n The vector width
   * @return A vectorized value
   */
  override def vectorize(n: ArithExpr): Value = Value(value, t.vectorize(n))
}

object Value {

  // implicit conversions from int, float, and tuples
  implicit def IntToValue(i: Int): Value = Value(i.toString, opencl.ir.Int)

  implicit def FloatToValue(f: Float): Value = Value(f.toString + "f", opencl.ir.Float)

  implicit def Tuple2ToValue[T1, T2](t: (T1, T2)): Value = {
    val tupleType = TupleType(getType(t._1), getType(t._2))
    Value(t.toString().replace('(', '{').replace(')', '}'), tupleType)
  }

  implicit def Tuple3ToValue[T1, T2, T3](t: (T1, T2, T3)): Value = {
    val tupleType = TupleType(getType(t._1), getType(t._2), getType(t._3))
    Value(t.toString().replace('(', '{').replace(')', '}'), tupleType)
  }

  private def getType(a: Any): Type = a match {
    case _: Float => Float
    case _: Int => Int
    case _ => throw new IllegalArgumentException
  }

  /** Factory methods for creating values */
  def apply(outT: Type): Value = {
    val v = Value("")
    v.t = outT
    v
  }

  def apply(value: String, outT: Type): Value = {
    val v = Value(value)
    v.t = outT
    v
  }

  def apply(v:Value, outT: Type): Value = {
    v.t = outT
    v
  }

  def apply(v: Value) = v

  @deprecated("Replaced by Value.vectorize(n)")
  def vectorize(v: Value, n: ArithExpr): Value = v.vectorize(n)
}


/** Function calls, ie.: map(f, x), zip(x, y), ...
  *
  * Refers back to the function decl (e.g. map(f)) and the arguments (e.g. x)
  */
sealed class FunCall(val f: FunDecl, val args: Expr*) extends Expr with Cloneable {

  assert(if (f.isInstanceOf[Iterate]) {
    this.isInstanceOf[IterateCall]
  } else {
    true
  })

  override def toString = {
    val fS = if (f == null) {
      "null"
    } else {
      f.toString
    }
    val argS = if (args.nonEmpty) args.map(_.toString).reduce(_ + ", " + _) else ""

    fS + "(" + argS + ")"
  }

  override def copy: FunCall = {
    this.clone().asInstanceOf[FunCall]
  }

  def apply(args: Expr*): FunCall = {
    val oldArgs = this.args
    val newArgs = oldArgs ++ args
    assert(newArgs.length <= f.params.length)

    new FunCall(f, newArgs: _*)
  }

  /** One type for all arguments (i.e. a tuple if there are more than one args)*/
  def argsType: Type = {
    if (args.length == 1) args(0).t
    else TupleType(args.map(_.t): _*)
  }

  def argsMemory: Memory = {
    if (args.length == 1) args(0).mem
    else OpenCLMemoryCollection(UndefAddressSpace, args.map(_.mem.asInstanceOf[OpenCLMemory]): _*)
  }

}
// specific types of function calls ...

case class MapCall(name: String, loopVar: Var,
                   override val f: AbstractMap, override val args: Expr*) extends FunCall(f, args.head) {
  assert(args.length == 1)

  var iterationCount: ArithExpr = ?

  def arg: Expr = args(0)
}

case class ReduceCall(loopVar: Var,
                      override val f: AbstractPartRed,
                      override val args: Expr*) extends FunCall(f, args.head, args(1)) {
  assert(args.length == 2)

  var iterationCount: ArithExpr = ?

  def arg0: Expr = args(0)

  def arg1: Expr = args(1)
}

case class IterateCall(override val f: Iterate, override val args: Expr*) extends FunCall(f, args.head) {
  assert(args.length == 1)

  def arg: Expr = args(0)

  var iterationCount: ArithExpr = ?

  var swapBuffer: Memory = UnallocatedMemory
  var indexVar = Var("i", RangeUnknown)
}
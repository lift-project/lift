package arithmetic

import lift.arithmetic._
import ir._
import ir.ast.Expr

import scala.collection.{immutable, mutable}
import scala.language.implicitConversions

/** a special variable that should only be used for defining function type*/
class TypeVar private(range : Range, fixedId: Option[Long] = None) extends ExtensibleVar("", range, fixedId) {
  override def copy(r: Range) = new TypeVar(r, Some(id))

  override def cloneSimplified() = new TypeVar(range, Some(id)) with SimplifiedExpr

  override def visitAndRebuild(f: (ArithExpr) => ArithExpr): ArithExpr =
    f(new TypeVar(range.visitAndRebuild(f), Some(id)))

  override lazy val toString = "tv_" + name + "_" + id
}

object TypeVar {

  def apply(range : Range = RangeUnknown) = {
    new TypeVar(range)
  }

  def getTypeVars(expr: Expr) : Set[TypeVar] = {
    Expr.visitWithState(immutable.HashSet[TypeVar]())(expr, (inExpr, set) => set ++ getTypeVars(inExpr.t))
  }

  def getTypeVars(t: Type) : Set[TypeVar] = {
    val result = new mutable.HashSet[TypeVar]()
    Type.visit(t, (ae:ArithExpr) => result ++= getTypeVars(ae) : Unit )
    result.toSet
  }

  def getTypeVars(expr: ArithExpr) : Set[TypeVar] = {
    val typeVars = scala.collection.mutable.HashSet[TypeVar]()
    ArithExpr.visit(expr, {
      case tv: TypeVar => typeVars += tv
      case _ =>
    })
    typeVars.toSet
  }
}

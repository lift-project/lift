package ir


import scala.collection.immutable
import scala.util.Random

class NotEvaluableException(msg: String) extends Exception(msg)


sealed abstract class Expr {

  def eval(): Int = {
    val dblResult = Expr.evalDouble(this)
    if (dblResult.isValidInt)
      dblResult.toInt
    else
      throw new NotEvaluableException("Cannot evaluate to int: "+dblResult)
  }

  def evalDbl(): Double = Expr.evalDouble(this)


  def *(that: Expr): Prod = {
    val thisExprs = this match {
      case p:Prod => p.terms
      case _ => List(this)
    }
    val thatExprs = that match {
      case p:Prod => p.terms
      case _ => List(that)
    }
    Prod(thisExprs++thatExprs)
  }

  def +(that: Expr): Sum = {
    val thisExprs = this match {
      case s:Sum => s.terms
      case _ => List(this)
    }
    val thatExprs = that match {
      case s:Sum => s.terms
      case _ => List(that)
    }
    Sum(thisExprs++thatExprs)
  }

  def /(that: Expr) = this * Pow(that, Cst(-1))
  //def -(that: Expr) = this + (that * Cst(-1))

}



object Expr {

  implicit def IntToCst(i: Int) = Cst(i)

  def getTypeVars(expr: Expr) : Set[TypeVar] = {
    val typeVars = scala.collection.mutable.HashSet[TypeVar]()
    visit(expr, {
      case tv: TypeVar => typeVars += tv
      case _ =>
    })
    typeVars.toSet
  }

  def contains(expr: Expr, elem: Expr) : Boolean = {
    var seen = false
    visit(expr, e => if (e==elem) seen=true)
    seen
  }

  def visit(e: Expr, f: (Expr) => Unit) : Unit = {
    f(e)
    e match {
      case Pow(base, exp) =>
        visit(base, f)
        visit(exp, f)
      case Sum(terms) => terms.foreach(t => visit(t, f))
      case Prod(terms) => terms.foreach(t => visit(t, f))
      case _ =>
    }
  }

  def substitute(e: Expr, substitutions: scala.collection.immutable.Map[Expr,Expr]) : Expr = {

    var newExpr = substitutions.getOrElse(e, e)

    newExpr = newExpr match {
      case Pow(l,r) => Pow(substitute(l,substitutions),substitute(r,substitutions))
      case adds: Sum => Sum(adds.terms.map(t => substitute(t, substitutions)))
      case muls: Prod => Prod(muls.terms.map(t => substitute(t, substitutions)))
      case _ => newExpr
    }

    ExprSimplifier.simplify(newExpr)
  }


  private def evalDouble(e: Expr) : Double = e match {
    case Cst(c) => c
    case Var(_,_) | ? | TypeVar(_) => throw new NotEvaluableException(e.toString)
    case Pow(base,exp) => scala.math.pow(evalDouble(base),evalDouble(exp))
    case Sum(terms) => terms.foldLeft(0.0)((result,expr) => result+evalDouble(expr))
    case Prod(terms) => terms.foldLeft(1.0)((result,expr) => result*evalDouble(expr))
  }



  def toInt(e: Expr): Int = {
    ExprSimplifier.simplify(e) match {
      case Cst(i) => i
      case _ => throw new NotEvaluableException(e.toString)
    }
  }

}

case object ? extends Expr
case class Cst(c: Int) extends Expr { override  def toString = c.toString }
case class Pow(b: Expr, e: Expr) extends Expr {
  override def toString : String = e match {
    case Cst(-1) => "1/("+b+")"
    case _ => "pow("+b+","+e+")"
  }
}
case class Prod(terms: List[Expr]) extends Expr {
  override def toString : String = {
    val m = if (terms.nonEmpty) { terms.map((t) => t.toString).reduce((s1, s2) => s1 + "*" + s2) } else {""}
    "(" + m +")"
  }
}
case class Sum(terms: List[Expr]) extends Expr {
  override def toString: String = "("+terms.map((t) => t.toString).reduce((s1, s2) => s1 + "+" + s2)+")"
}


// a special variable that should only be used for defining function type
case class TypeVar private(id: Int) extends Expr

object TypeVar {
  var cnt: Int = -1
  def apply() = {
    cnt = cnt+1
    new TypeVar(cnt)
  }
}

case class Var(name: String, var range : Range = RangeUnkown) extends Expr {

  override def equals(that: Any) = that match {
    case v: Var => this.name == v.name
    case _ => false
  }

  override def hashCode() = {
    val hash = 5
    hash * 79 + name.hashCode()
  }

  override def toString = name

}

object Var {
  var cnt: Int = -1

  def apply(range : Range) : Var = {
    cnt += 1
    Var("v"+cnt, range)
  }

  def setVarsAtRandom(vars : Set[Var]) : scala.collection.immutable.Map[Var, Cst] = {

    var changed = false
    var substitions : immutable.Map[Var, Cst] = new immutable.HashMap[Var, Cst]()
    var newVars : Set[Var] = vars

    do {
      changed = false

      // create a map of variable substitution
      val newSubsts : immutable.HashMap[Var, Cst] = newVars.foldLeft(immutable.HashMap[Var, Cst]())((map,v) => v.range match {
        case RangeAdd(Cst(start), Cst(stop), Cst(step)) => map+ (v -> Cst(Random.nextInt((stop - start) / step + 1) * step + start))
        case RangeMul(Cst(start), Cst(stop), Cst(mul))  => map+ (v -> Cst(start * math.pow(mul,Random.nextInt((math.log(stop / start) / math.log(mul) + 1).toInt)).toInt))
        case _ => map
      })

      if (newSubsts.nonEmpty)
        changed = true
      substitions = substitions ++ newSubsts

      // remove from the set of variables the ones which have a substitution
      newVars = newVars-- newSubsts.keySet

      // apply the substitutions in the range of each variable
      newVars.map(v => {
        v.range match {
          case RangeAdd(start, stop, step) => v.range = RangeAdd(
            ExprSimplifier.simplify(Expr.substitute(start, newSubsts.toMap)),
            ExprSimplifier.simplify(Expr.substitute(stop, newSubsts.toMap)),
            ExprSimplifier.simplify(Expr.substitute(step, newSubsts.toMap)))
          case RangeMul(start, stop, step) => v.range = RangeMul(
            ExprSimplifier.simplify(Expr.substitute(start, newSubsts.toMap)),
            ExprSimplifier.simplify(Expr.substitute(stop, newSubsts.toMap)),
            ExprSimplifier.simplify(Expr.substitute(step, substitions.toMap)))
          case _ =>
        }
        v
      })



    } while (changed)

    substitions
  }

  def getVars(f: Fun) : Set[Var] = {
    Fun.visit(immutable.HashSet[Var]())(f, (inF, set) => set ++ getVars(inF.inT))
  }

  def getVars(t: Type) : Set[Var] = {
    t match {
      case at: ArrayType => getVars(at.elemT) ++ getVars(at.len)
      case vt: VectorType => getVars(vt.len)
      case tt: TupleType => tt.elemsT.foldLeft(new immutable.HashSet[Var]())((set,inT) => set ++ getVars(inT))
      case _ => immutable.HashSet()
    }
  }

  def getVars(e: Expr) : Set[Var] = {
    e match {
      case adds: Sum => adds.terms.foldLeft(new immutable.HashSet[Var]())((set,expr) => set ++ getVars(expr))
      case muls: Prod => muls.terms.foldLeft(new immutable.HashSet[Var]())((set,expr) => set ++ getVars(expr))
      case v: Var => immutable.HashSet(v)
      case _ => immutable.HashSet()
    }
  }
}
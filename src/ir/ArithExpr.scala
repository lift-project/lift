package ir


import scala.collection.immutable
import scala.util.Random

class NotEvaluableException(msg: String) extends Exception(msg)


abstract class ArithExpr {

  def eval(): Int = {
    val dblResult = ArithExpr.evalDouble(this)
    if (dblResult.isValidInt)
      dblResult.toInt
    else
      throw new NotEvaluableException("Cannot evaluate " + this + " to int: "+ dblResult)
  }

  def evalDbl(): Double = ArithExpr.evalDouble(this)


  def *(that: ArithExpr): Prod = {
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

  def +(that: ArithExpr): Sum = {
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

  def /(that: ArithExpr) = this * Pow(that, Cst(-1))
  def -(that: ArithExpr) = this + (that * Cst(-1))

  def %(that: ArithExpr) = Mod(this, that)

  def &(that: ArithExpr) = And(this, that)

}



object ArithExpr {

  implicit def IntToCst(i: Int) = Cst(i)

  def max(e1: ArithExpr, e2: ArithExpr) : ArithExpr = {
    val diff = ExprSimplifier.simplify(e1 - e2)
    diff match {
      case Cst(c) => if (c < 0) e2 else e1
      case _ => throw new NotEvaluableException("Cannot determine max")
    }
  }

  def min(e1: ArithExpr, e2: ArithExpr) : ArithExpr = {
    val diff = ExprSimplifier.simplify(e1 - e2)
    diff match {
      case Cst(c) => if (c < 0) e1 else e2
      case _ => throw new NotEvaluableException("Cannot determine min")
    }
  }

  def max(e: ArithExpr) : ArithExpr = {
    e match {
      case _:Cst => e
      case Var(_, range) => if (range.max != ?) max(range.max) else e
      case Sum(sums) => Sum(sums.map(t => max(t)))

      // TODO: check if the product is positive or negative
      case Prod(prods) => Prod(prods.map(t => max(t)))

      case Pow(b, Cst(c)) => if (c>=0) Pow(max(b), Cst(c)) else Pow(min(b), Cst(c))

      case _ => throw new NotEvaluableException("Cannot determine max value")
    }
  }

  def min(e: ArithExpr) : ArithExpr = {
    e match {
      case _:Cst => e
      case Var(_, range) => if (range.min != ?) min(range.min) else e
      case Sum(sums) => Sum(sums.map(t => min(t)))

      // TODO: check if the product is positive or negative
      case Prod(prods) => Prod(prods.map(t => min(t)))

      case Pow(b, Cst(c)) => if (c>=0) Pow(min(b), Cst(c)) else Pow(max(b), Cst(c))

      case _ => throw new NotEvaluableException("Cannot determine min value")
    }
  }

  def getTypeVars(expr: ArithExpr) : Set[TypeVar] = {
    val typeVars = scala.collection.mutable.HashSet[TypeVar]()
    visit(expr, {
      case tv: TypeVar => typeVars += tv
      case _ =>
    })
    typeVars.toSet
  }

  def contains(expr: ArithExpr, elem: ArithExpr) : Boolean = {
    var seen = false
    visit(expr, e => if (e==elem) seen=true)
    seen
  }

  def visit(e: ArithExpr, f: (ArithExpr) => Unit) : Unit = {
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

  def substitute(e: ArithExpr, substitutions: scala.collection.immutable.Map[ArithExpr,ArithExpr]) : ArithExpr = {

    var newExpr = substitutions.getOrElse(e, e)

    newExpr = newExpr match {
      case Pow(l,r) => Pow(substitute(l,substitutions),substitute(r,substitutions))
      case adds: Sum => Sum(adds.terms.map(t => substitute(t, substitutions)))
      case muls: Prod => Prod(muls.terms.map(t => substitute(t, substitutions)))
      case _ => newExpr
    }

    ExprSimplifier.simplify(newExpr)
  }


  private def evalDouble(e: ArithExpr) : Double = e match {
    case Cst(c) => c
    case Var(_,_) | ? => throw new NotEvaluableException(e.toString)
    case Pow(base,exp) => scala.math.pow(evalDouble(base),evalDouble(exp))
    case Sum(terms) => terms.foldLeft(0.0)((result,expr) => result+evalDouble(expr))
    case Prod(terms) => terms.foldLeft(1.0)((result,expr) => result*evalDouble(expr))
  }



  def toInt(e: ArithExpr): Int = {
    ExprSimplifier.simplify(e) match {
      case Cst(i) => i
      case _ => throw new NotEvaluableException(e.toString)
    }
  }

  def asCst(e: ArithExpr) = {
    ExprSimplifier.simplify(e) match {
      case c:Cst => c
      case _ => throw new IllegalArgumentException
    }
  }

}

case object ? extends ArithExpr
case class Cst(c: Int) extends ArithExpr { override  def toString = c.toString }
case class Pow(b: ArithExpr, e: ArithExpr) extends ArithExpr {
  override def toString : String = e match {
    case Cst(-1) => "1/("+b+")"
    case _ => "pow("+b+","+e+")"
  }
}
case class Log(b: ArithExpr, x: ArithExpr) extends ArithExpr {
  override def toString: String = "log"+b+"("+x+")"
}

case class Prod(terms: List[ArithExpr]) extends ArithExpr {
  override def toString : String = {
    val m = if (terms.nonEmpty) { terms.map((t) => t.toString).reduce((s1, s2) => s1 + "*" + s2) } else {""}
    "(" + m +")"
  }
}
case class Sum(terms: List[ArithExpr]) extends ArithExpr {
  override def toString: String = "("+terms.map((t) => t.toString).reduce((s1, s2) => s1 + "+" + s2)+")"
}

case class Mod(dividend: ArithExpr, divisor: ArithExpr) extends ArithExpr {
  override def toString: String = "(" + dividend + " % " + divisor + ")"
}

case class And(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr {
  override def toString: String = "(" + lhs + " & " + rhs + ")"
}

// a special variable that should only be used for defining function type
class TypeVar private(range : Range) extends Var("", range) {
  override def toString = "t" + id
}

object TypeVar {
  //var cnt: Int = -1
  def apply(range : Range = RangeUnkown) = {
    //cnt = cnt+1
    new TypeVar(/*cnt, */range)
  }

  def getTypeVars(expr: Expr) : Set[TypeVar] = {
    Expr.visit(immutable.HashSet[TypeVar]())(expr, (inExpr, set) => set ++ getTypeVars(inExpr.inT))
  }

  def getTypeVars(t: Type) : Set[TypeVar] = {
    t match {
      case at: ArrayType => getTypeVars(at.elemT) ++ getTypeVars(at.len)
      case vt: VectorType => getTypeVars(vt.len)
      case tt: TupleType => tt.elemsT.foldLeft(new immutable.HashSet[TypeVar]())((set,inT) => set ++ getTypeVars(inT))
      case _ => immutable.HashSet()
    }
  }

  def getTypeVars(e: ArithExpr) : Set[TypeVar] = {
    e match {
      case adds: Sum => adds.terms.foldLeft(new immutable.HashSet[TypeVar]())((set,expr) => set ++ getTypeVars(expr))
      case muls: Prod => muls.terms.foldLeft(new immutable.HashSet[TypeVar]())((set,expr) => set ++ getTypeVars(expr))
      case v: TypeVar => immutable.HashSet(v)
      case _ => immutable.HashSet()
    }
  }
}

case class Var(name: String, var range : Range = RangeUnkown) extends ArithExpr {

  Var.cnt += 1
  val id: Int = Var.cnt

  override def equals(that: Any) = that match {
    case v: Var => this.id == v.id
    case _ => false
  }

  override def hashCode() = {
    val hash = 5
    hash * 79 + id;
  }

  override def toString = name

  def updateRange(func: (Range) => Range): Unit = {
    if (range != RangeUnkown) {
      range = func(range)
    }
  }

}

object Var {
  var cnt: Int = -1

  def apply(range : Range) : Var = new Var("",range)


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
            ExprSimplifier.simplify(ArithExpr.substitute(start, newSubsts.toMap)),
            ExprSimplifier.simplify(ArithExpr.substitute(stop, newSubsts.toMap)),
            ExprSimplifier.simplify(ArithExpr.substitute(step, newSubsts.toMap)))
          case RangeMul(start, stop, step) => v.range = RangeMul(
            ExprSimplifier.simplify(ArithExpr.substitute(start, newSubsts.toMap)),
            ExprSimplifier.simplify(ArithExpr.substitute(stop, newSubsts.toMap)),
            ExprSimplifier.simplify(ArithExpr.substitute(step, substitions.toMap)))
          case _ =>
        }
        v
      })



    } while (changed)

    substitions
  }

  def getVars(expr: Expr) : Set[Var] = {
    Expr.visit(immutable.HashSet[Var]())(expr, (inExpr, set) => set ++ getVars(inExpr.inT))
  }

  def getVars(t: Type) : Set[Var] = {
    t match {
      case at: ArrayType => getVars(at.elemT) ++ getVars(at.len)
      case vt: VectorType => getVars(vt.len)
      case tt: TupleType => tt.elemsT.foldLeft(new immutable.HashSet[Var]())((set,inT) => set ++ getVars(inT))
      case _ => immutable.HashSet()
    }
  }

  def getVars(e: ArithExpr) : Set[Var] = {
    e match {
      case adds: Sum => adds.terms.foldLeft(new immutable.HashSet[Var]())((set,expr) => set ++ getVars(expr))
      case muls: Prod => muls.terms.foldLeft(new immutable.HashSet[Var]())((set,expr) => set ++ getVars(expr))
      case v: Var => immutable.HashSet(v)
      case _ => immutable.HashSet()
    }
  }
}
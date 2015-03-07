package ir


import scala.collection.immutable
import scala.util.Random

class NotEvaluableException(msg: String) extends Exception(msg)


abstract sealed class ArithExpr {

  def eval(): Int = {
    val dblResult = ArithExpr.evalDouble(this)
    if (dblResult.isValidInt)
      dblResult.toInt
    else
      throw new NotEvaluableException("Cannot evaluate " + this + " to int: "+ dblResult)
  }

  def evalDbl(): Double = ArithExpr.evalDouble(this)

  def evalAtMax(): Int = {
    val e: ArithExpr = atMax
    e.eval()
  }

  def atMax: ArithExpr = {
    val vars = Var.getVars(this)
    val maxLens = vars.map(_.range.max)
    ArithExpr.substitute(this, (vars, maxLens).zipped.toMap)
  }

  def *(that: ArithExpr): Prod = {
    val thisExprs = this match {
      case p:Prod => p.factors
      case _ => List(this)
    }
    val thatExprs = that match {
      case p:Prod => p.factors
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

  def div(that: ArithExpr) = Fraction(this, that)
  def /(that: ArithExpr) = this * Pow(that, Cst(-1))
  def -(that: ArithExpr) = this + (that * Cst(-1))

  def %(that: ArithExpr) = Mod(this, that)

  def &(that: ArithExpr) = And(this, that)

}



object ArithExpr {

  implicit def IntToCst(i: Int): Cst = Cst(i)

  def max(e1: ArithExpr, e2: ArithExpr) : ArithExpr = {
    minmax(e1, e2)._2
  }

  def min(e1: ArithExpr, e2: ArithExpr) : ArithExpr = {
    minmax(e1, e2)._1
  }

  def minmax(v: Var, c: Cst): (ArithExpr, ArithExpr) = {
    val m1 = v.range.min match { case Cst(min) => if (min >= c.c) Some((c, v)) else None }
    if (m1.isDefined) return m1.get

    val m2 = v.range.max match { case Cst(max) => if (max <= c.c) Some((v, c)) else None }
    if (m2.isDefined) return m2.get

    throw new NotEvaluableException("Cannot determine min/max of " + v + " and " + c)
  }

  def minmax(p: Prod, c: Cst): (ArithExpr, ArithExpr) = {
    val lb = lowerBound(p)
    if (lb.isDefined && lb.get >= c.c) return (c, p)

    val ub = upperBound(p)
    if (ub.isDefined && ub.get <= c.c) return (p, c)

    throw new NotEvaluableException("Cannot determine min/max of " + p + " and " + c)
  }

  private def upperBound(p: Prod): Option[Int] = {
    Some(Prod(p.factors.map({
      case v: Var => v.range.max match {
        case max: Cst => max
        case _ => return None
      }
      case c: Cst => c
    })).eval())
  }

  private def lowerBound(p: Prod): Option[Int] = {
    Some(Prod(p.factors.map({
      case v: Var => v.range.min match {
        case min: Cst => min
        case _ => return None
      }
      case c: Cst => c
    })).eval())
  }

  def minmax(e1: ArithExpr, e2: ArithExpr): (ArithExpr, ArithExpr) = {
    val diff = ExprSimplifier.simplify(e1 - e2)
    diff match {
      case Cst(c) => if (c < 0) (e1, e2) /* e1 is smaller than e2 */ else (e2, e1) /* e2 is smaller than e1*/
      case _ =>
        (e1, e2) match {
          case (v: Var, c: Cst) => minmax(v, c)
          case (c: Cst, v: Var) => val m = minmax(v, c); (m._2, m._1)

          case (p: Prod, c: Cst) => minmax(p, c)
          case (c: Cst, p: Prod) => val m = minmax(p, c); (m._2, m._1)

          case _ =>
            throw new NotEvaluableException("Cannot determine min/max of " + e1 + " and " + e2)
        }
    }
  }

  def max(e: ArithExpr) : ArithExpr = minmax(e)._2

  def min(e: ArithExpr) : ArithExpr = minmax(e)._1

  def minmax(e: ArithExpr): (ArithExpr, ArithExpr) = {
    e match {
      case _: Cst => (e, e)
      case Var(_, range) => ( if (range.min != ?) min(range.min) else e,
                              if (range.max != ?) max(range.max) else e )

      case Sum(sums) => ( Sum(sums.map(min)), Sum(sums.map(max)) )

      // TODO: check if the product is positive or negative
      case Prod (prods) => ( Prod(prods.map(min)), Prod(prods.map(max)) )

      case Pow(b, Cst(c)) => ( if (c>=0) Pow(min(b), Cst(c)) else Pow(max(b), Cst(c)),
                               if (c>=0) Pow(max(b), Cst(c)) else Pow(min(b), Cst(c)) )

      case _ =>  throw new NotEvaluableException("Cannot determine min/max values")
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
      case muls: Prod => Prod(muls.factors.map(t => substitute(t, substitutions)))
      case _ => newExpr
    }

    ExprSimplifier.simplify(newExpr)
  }


  private def evalDouble(e: ArithExpr) : Double = e match {
    case Cst(c) => c
    case Var(_,_) | ArithExprFunction() | ? => throw new NotEvaluableException(e.toString)

    case Fraction(n, d) => scala.math.floor(evalDouble(n) / evalDouble(d))

    case Pow(base,exp) => scala.math.pow(evalDouble(base),evalDouble(exp))
    case Log(b,x) => scala.math.log(evalDouble(x)) / scala.math.log(evalDouble(b))

    case Mod(dividend, divisor) => dividend.eval % divisor.eval

    case And(l,r) => l.eval & r.eval

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
object jCst { def create(c: Int) = Cst(c) }

case class Fraction(numer: ArithExpr, denom: ArithExpr) extends ArithExpr {
  override def toString: String = "("+ numer + " div " + denom +")"
}

case class Pow(b: ArithExpr, e: ArithExpr) extends ArithExpr {
  override def toString : String = e match {
    case Cst(-1) => "1/("+b+")"
    case _ => "pow("+b+","+e+")"
  }
}
case class Log(b: ArithExpr, x: ArithExpr) extends ArithExpr {
  override def toString: String = "log"+b+"("+x+")"
}

case class Prod(factors: List[ArithExpr]) extends ArithExpr {
  override def equals(that: Any) = that match {
    case p: Prod => factors.length == p.factors.length && factors.intersect(p.factors).length == factors.length
    case _ => false
  }

  override def toString : String = {
    val m = if (factors.nonEmpty) factors.map((t) => t.toString).reduce((s1, s2) => s1 + "*" + s2) else {""}
    "(" + m +")"
  }
}
case class Sum(terms: List[ArithExpr]) extends ArithExpr {
  override def equals(that: Any) = that match {
    case s: Sum => terms.length == s.terms.length && terms.intersect(s.terms).length == terms.length
    case _ => false
  }

  override def toString: String = {
    val m = if (terms.nonEmpty) terms.map((t) => t.toString).reduce((s1, s2) => s1 + "+" + s2) else {""}
    "(" + m +")"
  }
}

case class Mod(dividend: ArithExpr, divisor: ArithExpr) extends ArithExpr {
  override def toString: String = "(" + dividend + " % " + divisor + ")"
}

case class And(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr {
  override def toString: String = "(" + lhs + " & " + rhs + ")"
}

case class Floor(ae : ArithExpr) extends ArithExpr {
  override def toString: String = "Floor(" + ae + ")"
}

case class ArithExprFunction() extends ArithExpr

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
    Expr.visit(immutable.HashSet[TypeVar]())(expr, (inExpr, set) => set ++ getTypeVars(inExpr.t))
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
      case muls: Prod => muls.factors.foldLeft(new immutable.HashSet[TypeVar]())((set,expr) => set ++ getTypeVars(expr))
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
    hash * 79 + id
  }

  override def toString = if (name == "") "v_"+id else name

  def updateRange(func: (Range) => Range): Unit = {
    if (range != RangeUnkown) {
      range = func(range)
    }
  }

}

object jVar {
  def create(name: String, range: Range) = Var(name, range)
  def create(name: String) = Var(name)
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

  def getVars(expr: Expr) : Seq[Var] = {
    Expr.visit(Seq[Var]())(expr, (inExpr, set) => set ++ getVars(inExpr.t))
  }

  def getVars(t: Type) : Seq[Var] = {
    t match {
      case at: ArrayType => getVars(at.elemT) ++ getVars(at.len)
      case vt: VectorType => getVars(vt.len)
      case tt: TupleType => tt.elemsT.foldLeft(Seq[Var]())((set,inT) => set ++ getVars(inT))
      case _ => Seq[Var]().distinct
    }
  }

  def getVars(e: ArithExpr) : Seq[Var] = {
    e match {
      case adds: Sum => adds.terms.foldLeft(Seq[Var]())((set,expr) => set ++ getVars(expr))
      case muls: Prod => muls.factors.foldLeft(Seq[Var]())((set,expr) => set ++ getVars(expr))
      case v: Var => Seq(v)
      case _ => Seq[Var]()
    }
  }
}

object SizeVar {
  def apply(name: String): Var = {
    Var(name, StartFromRange(Cst(1)))
  }
}

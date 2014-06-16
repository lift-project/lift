package ir


import scala.collection.immutable
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.immutable.Set
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

  //def simplify() = this

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
  def -(that: Expr) = this + (that * Cst(-1))

}


object Expr {

  private def evalDouble(e: Expr) : Double = e match {
    case Cst(c) => c
    case Var(_,_) | ? => throw new NotEvaluableException(e.toString)
    case Pow(base,exp) => scala.math.pow(evalDouble(base),evalDouble(exp))
    case Sum(terms) => terms.foldLeft(0.0)((result,expr) => result+evalDouble(expr))
    case Prod(terms) => terms.foldLeft(1.0)((result,expr) => result*evalDouble(expr))
  }

  private def simplifyPow(pow: Pow): Expr = {
    pow match {
      case Pow(Cst(0), Cst(0)) => throw new NotEvaluableException(pow.toString)
      case Pow(base, Cst(0)) => Cst(1)
      case Pow(base, Cst(1)) => base
      case Pow(Cst(0), _) => Cst(0)
      case Pow(Cst(1), _) => Cst(1)
      case Pow(Prod(terms), exp) => simplifyProd(Prod(terms.map(t => simplifyPow(Pow(t, exp)))))
      case Pow(_,_) => pow
    }
  }

  private def simplifySum(sum: Sum): Expr = sum match {
    case Sum(terms) => {

      if (terms.length == 1)
        return terms(0)

      var cst : Double = 0
      var result : Expr = Sum(List())
      terms.map(t => t match {
        case Cst(c) => cst = cst+c
        case _ => result = result + t
      })

      if (cst != 0)
        if (cst.isValidInt)
          if (result == Sum(List()))
            result = Cst(cst.toInt)
          else
            result = Cst(cst.toInt) + result // constants always first
        else
          throw new NotEvaluableException(result.toString)

      result
    }
  }

  private def simplifyProd(prod: Prod): Expr = prod match {

    case Prod(terms) => {

      if (terms.length == 1)
        return terms(0)

      var cst : Double = 1
      var sums = Set[Sum]()
      var result : Expr = Prod(List())

      terms.map(t => t match {
        case Cst(c) => cst = cst*c
        case Pow(Cst(b),Cst(e)) => cst = cst * scala.math.pow(b,e)
        case s: Sum => sums = sums+s
        case _ => result = result * t
      })

      // constant folding
      if (cst != 1)
        if (cst.isValidInt)
          if (result == Prod(List()))
            result = Cst(cst.toInt)
          else
            result = Cst(cst.toInt) * result // constants always first
        else
          throw new NotEvaluableException(result.toString)

      // distributivity
      if (!sums.isEmpty) {
        if (result != Prod(List()))
          sums = sums + Sum(List(result))
        result = simplifySum(sums.reduce((s1, s2) => Sum(s1.terms.map(t1 => s2.terms.map(t2 => simplifyProd(t1 * t2))).flatten)))
      }
      result
    }

    /*case Prod(terms) => {
      val baseExpMap = terms.foldLeft(new HashMap[Expr, Expr]())((map, e) => {
        e match {
          case Pow(b, e) => map + (b -> simplifySum(map.getOrElse(b, Cst(0)) + e))
          case _ => map + (e -> simplifySum(map.getOrElse(e, Cst(0)) + Cst(1)))
        }
      })
      var cst: Double = 0
      var result: Expr = Prod(List())
      baseExpMap.map({ case (base, exp) => {
        val t = simplifyPow(Pow(base, exp))
        t match {
          case Cst(c) => cst = cst * c
          case _ => result = result * t
        }
      }
      })

      if (cst != 0)
        if (cst.isValidInt)
          result = Cst(cst.toInt) * result // constants always first
        else
          throw new NotEvaluableException(result.toString)

      result*/

  }

  def simplify(e: Expr): Expr = {

        // recurse inside first
        var result = e match {
          case Pow(base, exp) => Pow(simplify(base), simplify(exp))
          case Prod(terms) => Prod(terms.map(t => simplify(t)))
          case Sum(terms) => Sum(terms.map(t => simplify(t)))
          case _ => e
        }

        result = result match {
          case p: Pow => simplifyPow(p)
          case p: Prod => simplifyProd(p)
          case s: Sum => simplifySum(s)
          case _ => result
        }

        result
      }
  }

case object ? extends Expr
case class Cst(c: Int) extends Expr
case class Pow(b: Expr, e: Expr) extends Expr
case class Prod(terms: List[Expr]) extends Expr
case class Sum(terms: List[Expr]) extends Expr



case class Var private(name: String, var range : Range) extends Expr {

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

  def apply(range : Range = RangeUnkown) : Var = {
    cnt += 1
    Var("v"+cnt, range)
  }

  def apply(name: String) : Var = {
    Var(name, RangeUnkown)
  }

  def setVarsAtRandom(vars : Set[Var]) : scala.collection.immutable.Map[Var, Cst] = {

    var changed = false
    var substitions : immutable.Map[Var, Cst] = new HashMap[Var, Cst]()
    var newVars : Set[Var] = vars

    do {
      changed = false

      // create a map of variable substitution
      val newSubsts : HashMap[Var, Cst] = newVars.foldLeft(HashMap[Var, Cst]())((map,v) => v.range match {
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
            Expr.simplify(substitute(start, newSubsts)),
            Expr.simplify(substitute(stop, newSubsts)),
            Expr.simplify(substitute(step, newSubsts)))
          case RangeMul(start, stop, step) => v.range = RangeMul(
            Expr.simplify(substitute(start, newSubsts)),
            Expr.simplify(substitute(stop, newSubsts)),
            Expr.simplify(substitute(step, substitions)))
          case _ =>
        }
        v
      })



    } while (changed)

    substitions
  }

  def substitute(e: Expr, substitutions: scala.collection.immutable.Map[Var, Cst]) : Expr = {
    val newExpr = e match {
      case Pow(l,r) => Pow(substitute(l,substitutions),substitute(r,substitutions))
      case adds: Sum => Sum(adds.terms.map(t => substitute(t, substitutions)))
      case muls: Prod => Prod(muls.terms.map(t => substitute(t, substitutions)))
      case v: Var => substitutions.getOrElse(v, v)
      case _ => e
    }
    Expr.simplify(newExpr)
  }

  def getVars(f: Fun) : Set[Var] = {
    Fun.visit(HashSet[Var]())(f, (inF, set) => set ++ getVars(inF.inT))
  }

  def getVars(t: Type) : Set[Var] = {
    t match {
      case at: ArrayType => getVars(at.elemT) ++ getVars(at.len)
      case tt: TupleType => tt.elemsT.foldLeft(new HashSet[Var]())((set,inT) => set ++ getVars(inT))
      case _ => HashSet()
    }
  }

  def getVars(e: Expr) : Set[Var] = {
    e match {
      case adds: Sum => adds.terms.foldLeft(new HashSet[Var]())((set,expr) => set ++ getVars(expr))
      case muls: Prod => muls.terms.foldLeft(new HashSet[Var]())((set,expr) => set ++ getVars(expr))
      case v: Var => HashSet(v)
      case _ => HashSet()
    }
  }
}
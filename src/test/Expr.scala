package test


import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.immutable.Set
import scala.util.Random

sealed abstract class Expr {
  def simplify() = this
  def *(that: Expr) = new Mul(this, that)
  def /(that: Expr) = new Mul(this, Pow(that, Cst(-1)))
  def +(that: Expr) : Expr = new Add(this, that)
  def -(that: Expr) = new Add(this, Mul(Cst(-1),that))
  def ^(that: Expr) = new Pow(this, that)  
}

object Expr {
  

  
  def simplify(e: Expr) : Expr = {    
    
     val simpExpr = e match {           
       case Mul(l,r) => Mul(simplify(l),simplify(r))
       case Add(l,r) => Add(simplify(l),simplify(r))
       case Pow(b,e) => Pow(simplify(b),simplify(e))
       case Prod(terms) => Prod(terms.map({case (e,i) => (simplify(e)->i)})) 
       case Sum(terms)  => Sum(terms.map({case (e,i) => (simplify(e)->i)})) 
       case _ => e
     }

     val prodSumExpr = simpExpr match {
      case Mul(l: Prod, r: Prod) => l + r
      case Mul(l: Prod, r) => l + r
      case Mul(l, r: Prod) => l + r
      case Mul(l, r) => new Prod()+l+r
      case Add(l: Sum, r: Sum) => l + r
      case Add(l: Sum, r) => l + r
      case Add(l, r: Sum) => l + r
      case Add(l, r) => new Sum()+l+r
      case _ => simpExpr
    }
     
    val simpProdSumExpr = prodSumExpr match {
      case p : Prod => p.simplify
      case s : Sum  => s.simplify
      case _ => prodSumExpr
    }
    
    // the simplified sum/prod might create a Pow
    simpProdSumExpr match {
      case Pow(Cst(0),Cst(0)) => simpProdSumExpr // illegal
      //case Pow(b: Cst,e: Cst) => Cst(math.pow(b.cst ,e.cst).toInt) // cannot do this here, otherwise imprecision will appear
      case Pow(Cst(0), e) => Cst(0)
      case Pow(Cst(1), e) => Cst(1)
      case Pow(b, Cst(0)) => Cst(1)
      case Pow(b, Cst(1)) => b
      case _ => simpProdSumExpr
    }
  }
  
}

private case class Prod(val terms: HashMap[Expr,Int]) extends Expr {
  def this() = this(new HashMap)

  override def toString() = {
    "("+terms.map(keyval =>
      if (keyval._2 == 1)
        keyval._1.toString
      else
        keyval._1.toString + "^" + keyval._2).reduce((s1, s2) => s1 + "*" + s2)+")"
  }  
  
  override def simplify() : Expr = {
    
    var result = terms//.map({case (e,i) => (Expr.simplify(e),i)})
        
    // simplify all constant terms into one
    val cstTerms = terms.filterKeys({
      e => e match {
        case _:Cst => true
        case Pow(Cst(_),Cst(_)) => true
        case _ => false
        }})
    val cstD = cstTerms.foldLeft(1.0)((acc,t) => t match {
      case (Cst(c),i) => acc*math.pow(c,i)
      case (Pow(Cst(b),Cst(e)),i) => acc*math.pow(b, e*i)
      })
    // TODO: check cstD is an integer number 
    val cstExpr = Cst(cstD.toInt)
    if (cstExpr.cst == 0) // result is 0, nothing else to do
      return cstExpr        
    result = (terms--cstTerms.keySet)+(cstExpr->1)
    
    // if we only have one results, return it
    if (result.size == 1) {
      val t = result.last
      return t._1^Cst(t._2)
    }
    this      
  }
  
  override def +(t: Expr) : Expr = {
   val reverse = t match {
      case Pow(reverse,Cst(-1)) => reverse
      case _ => Pow(t, Cst(-1))
    }

    val cnt = terms.get(reverse)
    if (!cnt.isEmpty)
      if (cnt.get == 1)
        return Prod(terms - reverse)
      else
        return Prod(terms + (reverse -> (cnt.get - 1)))
        
	Prod(terms+(t->(terms.getOrElse(t, 0)+1)))
  }
  def +(that: Prod) : Prod = {
    throw new NotImplementedError() 
  }
}
private case class Sum(val terms: HashMap[Expr,Int]) extends Expr {
  def this() = this(new HashMap)
  
  override def toString() = {
    "("+
    terms.map(keyval =>
      if (keyval._2 == 1)
        keyval._1.toString
      else
        keyval._1.toString + "*" + keyval._2).reduce((s1, s2) => s1 + "+" + s2) +")"
  }  
    
  override def simplify() : Expr = {
    
    var result = terms//.map({case (e,i) => (Expr.simplify(e),i)})
        
    // simplify all constant terms into one
    val cstTerms = terms.filterKeys({
      e => e match {
        case _:Cst => true
        case Pow(Cst(_),Cst(_)) => true        
        case _ => false
        }})
    val cstD = cstTerms.foldLeft(0.0)((acc,t) => t match {
      case (Cst(c),i) => acc+math.pow(c,i)
      case (Pow(Cst(b),Cst(e)),i) => acc+math.pow(b, e*i)
      })
    // TODO: check cstD is an integer number 
    val cstExpr = Cst(cstD.toInt)
    result = (terms--cstTerms.keySet)+(cstExpr->1)
    
    // if we only have one results, return it
    if (result.size == 1) {
      val t = result.last
      return t._1^Cst(t._2)
    }
    this      
  }
  
  override def +(t: Expr) : Expr = {
   val reverse = t match {
      case Mul(reverse,Cst(-1)) => reverse
      case Mul(Cst(-1),reverse) => reverse
      case _ => Mul(Cst(-1), t)
    }

    val cnt = terms.get(reverse)
    if (!cnt.isEmpty)
      if (cnt.get == 1)
        return Sum(terms - reverse)
      else
        return Sum(terms + (reverse -> (cnt.get - 1)))
        
	Sum(terms+(t->(terms.getOrElse(t, 0)+1)))
  }
  def +(that: Sum) : Sum = {
    throw new NotImplementedError() 
  }
}

// undefined
case object ? extends Expr



case class Cst(val cst: Int) extends Expr {override def toString() = cst.toString}

case class Var private(val id: String, var range : Range) extends Expr {
  
  override def equals(that: Any) = that match {
    case v: Var => this.id == v.id
    case _ => false
  }
 
  override def hashCode() = {
    val hash = 5
    hash * 79 + id.hashCode() 
  }
    
  override def toString() = id
  
}
  
object Var {
  var cnt: Int = -1
  def apply(range : Range) : Var = {
    cnt += 1;
    Var("v"+cnt, range)
  }
  
  def setVarsAtRandom(vars : Set[Var]) : scala.collection.immutable.Map[Var, Cst] = {
    
    var changed = false
    var substitions = new HashMap[Var, Cst]()
    var newVars : Set[Var] = vars

    do {      
      changed = false
      
      // create a map of variable substitution
      val newSubsts = newVars.foldLeft(HashMap[Var, Cst]())((map,v) => v.range match {
        case RangeAdd(Cst(start), Cst(stop), Cst(step)) => map+ (v -> Cst(Random.nextInt((stop - start) / step + 1) * step + start))
        case RangeMul(Cst(start), Cst(stop), Cst(mul))  => map+ (v -> Cst(start * math.pow(mul,Random.nextInt((math.log(stop / start) / math.log(mul) + 1).toInt)).toInt))
        case _ => map
      })
      
      if (newSubsts.nonEmpty)
        changed = true
      substitions = substitions ++ newSubsts
      
      
      /*substitions = newVars.map(v => v.range match {
        case RangeAdd(Cst(start), Cst(stop), Cst(step)) => Some((v, Cst(Random.nextInt((stop - start) / step + 1) * step + start)))
        case RangeMul(Cst(start), Cst(stop), Cst(mul)) => Some((v,
            //if ((math.log(stop / start) / math.log(mul) + 1).toInt < 1) {
            //  println("err!" + (math.log(stop / start) / math.log(mul) + 1).toInt);
            //  Cst(0)
            //} else
            	Cst(start * math.pow(mul,Random.nextInt((math.log(stop / start) / math.log(mul) + 1).toInt)).toInt)))
        case _ => None
      }).foldRight(HashMap[Var, Cst]())((opt, map) => if (opt.isDefined) { changed = true; map + opt.get } else map)*/

      //println(substitions)          
      
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
      case Mul(l,r) => Mul(substitute(l,substitutions),substitute(r,substitutions))
      case Add(l,r) => Add(substitute(l,substitutions),substitute(r,substitutions))
      case Pow(l,r) => Pow(substitute(l,substitutions),substitute(r,substitutions))
      case adds: Sum => Sum(adds.terms.map({case (expr, pow) => (substitute(expr, substitutions),pow) }))
      case muls: Prod => Prod(muls.terms.map({case (expr, pow) => (substitute(expr, substitutions),pow) }))
      case v: Var => substitutions.getOrElse(v, v)      
      case _ => e
    }
    Expr.simplify(newExpr)
  }
  
  def getVars(f: Fun) : Set[Var] = {
    Fun.visit(HashSet[Var]())(f, ((inF,set) => set++getVars(inF.inT)))
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
      case bo: BinOp => getVars(bo.l) ++ getVars(bo.r)
      case adds: Sum => adds.terms.keySet.foldLeft(new HashSet[Var]())((set,expr) => set ++ getVars(expr))
      case muls: Prod => muls.terms.keySet.foldLeft(new HashSet[Var]())((set,expr) => set ++ getVars(expr)) 
      case v: Var => HashSet(v)
      case _ => HashSet()
    }
  }
}

abstract class BinOp(val l: Expr, val r: Expr, val op: String) extends Expr {
  override def toString() = {"("+l+op+r+")"}
}

case class Mul(override val l: Expr, override val r: Expr) extends BinOp(l,r,"*")
case class Add(override val l: Expr, override val r: Expr) extends BinOp(l,r,"+")
case class Pow(override val l: Expr, override val r: Expr) extends BinOp(l,r,"^")


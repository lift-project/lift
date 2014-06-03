package test

import scala.collection.immutable.HashMap

sealed abstract class Expr {
  def simplify() = this
  def /(that: Expr) = new Div(this, that)
  def *(that: Expr) = new Mul(this, that)
  def +(that: Expr) : Expr = new Add(this, that)
  def -(that: Expr) = new Sub(this, that)  
  def ^(that: Expr) = new Pow(this, that)  
}

object Expr {
  
  def simplify(e: Expr) : Expr = {
    e match {

    	case Sub(l,Cst(0)) => simplify(l)
    	case Sub(l,r)      => simplify(Add(l, Mul(Cst(-1),r)))
    	case Div(l,Cst(1)) => simplify(l)
    	case Div(l,r)      => simplify(Mul(l, Pow(r,Cst(-1))))
    
    	case Add(Cst(0),r) => simplify(r)
    	case Add(l,Cst(0)) => simplify(l)

    	case Add(a1:Adds, a2:Adds) => a1+a2
    	case Add(a,adds:Adds) => adds+simplify(a)
    	case Add(adds:Adds,a) => adds+simplify(a)
    	case Add(l,r)      => new Adds()+simplify(l)+simplify(r)   	
    	
    	case Mul(Cst(0),r) => Cst(0)
    	case Mul(l,Cst(0)) => Cst(0)
    	case Mul(Cst(1),r) => simplify(r)
    	case Mul(l,Cst(1)) => simplify(l)

    	case Mul(a1:Muls, a2:Muls) => a1+a2
    	case Mul(a,muls:Muls)      => muls+simplify(a)
    	case Mul(muls:Muls,a)      => muls+simplify(a)
    	case Mul(l,r)              => new Muls()+simplify(l)+simplify(r) 

    	
    	case Pow(b,Cst(1)) => simplify(b)
    	case Pow(Cst(0),e) => simplify(e)
    	case Pow(b,e)      => Pow(simplify(b),simplify(e))   	    	

    	case _ => e
    }
  }
}

private case class Muls(val terms: HashMap[Expr,Int]) extends Expr {
  def this() = this(new HashMap)

  override def toString() = {
    terms.map(keyval =>
      if (keyval._2 == 1)
        keyval._1.toString
      else
        keyval._1.toString + "^" + keyval._2).reduce((s1, s2) => s1 + "*" + s2)
  }  
  
  override def +(t: Expr) : Expr = {
   val reverse = t match {
      case Pow(reverse,Cst(-1)) => reverse
      case _ => Pow(t, Cst(-1))
    }

    val cnt = terms.get(reverse)
    if (!cnt.isEmpty)
      if (cnt.get == 1)
        return Muls(terms - reverse)
      else
        return Muls(terms + (reverse -> (cnt.get - 1)))
        
	Muls(terms+(t->(terms.getOrElse(t, 0)+1)))
  }
  def +(that: Muls) {
    new NotImplementedError() 
  }
}
private case class Adds(val terms: HashMap[Expr,Int]) extends Expr {
  def this() = this(new HashMap)
  
  override def toString() = {
    terms.map(keyval =>
      if (keyval._2 == 1)
        keyval._1.toString
      else
        keyval._1.toString + "*" + keyval._2).reduce((s1, s2) => s1 + "+" + s2)
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
        return Adds(terms - reverse)
      else
        return Adds(terms + (reverse -> (cnt.get - 1)))
        
	Adds(terms+(t->(terms.getOrElse(t, 0)+1)))
  }
  def +(that: Adds) {
    new NotImplementedError() 
  }
}

// undefined
case object ? extends Expr



case class Cst(val cst: Int) extends Expr {override def toString() = cst.toString}

case class Var private(val id: String, val range : Range) extends Expr {
  
  override def toString() = id
  
}
  
object Var {
  var cnt: Int = -1
  def apply(range : Range) : Var = {
    cnt += 1;
    Var("v"+cnt, range)
  }
}

case class Div(val l: Expr, val r: Expr) extends Expr {override def toString() = l+"/"+r}
case class Mul(val l: Expr, val r: Expr) extends Expr {override def toString() = l+"*"+r}
case class Add(val l: Expr, val r: Expr) extends Expr {override def toString() = l+"+"+r}
case class Sub(val l: Expr, val r: Expr) extends Expr {override def toString() = l+"-"+r}
case class Pow(val l: Expr, val r: Expr) extends Expr {override def toString() = l+"^"+r}


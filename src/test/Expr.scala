package test

sealed abstract class Expr {
  def simplify() = this
  def /(that: Expr) = new Div(this, that)
  def *(that: Expr) = new Mul(this, that)
  def +(that: Expr) = new Add(this, that)
  def -(that: Expr) = new Sub(this, that)  
  def ^(that: Expr) = new Pow(this, that)  
}

object Expr {
 /* def simplify(e: Expr) : Expr = {
    e match {
    	case Add(Cst(0),r) => simplify(r)
    	case Add(l,Cst(0)) => simplify(l)
    	case Add(l,r)      => Add(simplify(l), simplify(r))
    	case Sub(l,Cst(0)) => simplify(l)   
    	case Sub(l,r)      => Sub(simplify(l), simplify(r))
    	case Mul(Cst(0),r) => Cst(0)
    	case Mul(l,Cst(0)) => Cst(0)
    	case Mul(Cst(1),r) => simplify(r)
    	case Mul(l,Cst(1)) => simplify(l)
    	case Mul(l,r)      => Mul(simplify(l), simplify(r))
    	case Div(l,Cst(1)) => l  
    	case Div(l,r)      => Div(simplify(l), simplify(r))
    	
    	case Mul(Mul(a,b),c) => Terms(Set(a,b,c))
    	case Mul(a, Mul(b,c)) => Terms(Set(a,b,c))
    	case Mul(Terms(terms1),Terms(terms2)) => Terms(terms1++terms2)
    	case Mul(a,Terms(terms)) => Terms(terms+a)
    	case Mul(Terms(terms),a) => Terms(terms+a)

    	case _ => e
    }
  }*/
  
  def simplify(e: Expr) : Expr = {
    e match {

    	case Sub(l,Cst(0)) => simplify(l)
    	case Sub(l,r)      => simplify(Add(l, Mul(Cst(-1),r)))
    	case Div(l,Cst(1)) => simplify(l)
    	case Div(l,r)      => simplify(Mul(l, Pow(r,Cst(-1))))
    
    	case Add(Cst(0),r) => simplify(r)
    	case Add(l,Cst(0)) => simplify(l)
    	case Add(Add(a,b),c) => Adds(Set(simplify(a),simplify(b),simplify(c)))
    	case Add(a, Add(b,c)) => Adds(Set(simplify(a),simplify(b),simplify(c)))
    	case Add(Adds(terms1),Adds(terms2)) => Adds(terms1++terms2)
    	case Add(a,Adds(terms)) => Adds(terms+simplify(a))
    	case Add(Adds(terms),a) => Adds(terms+simplify(a))    	
    	case Add(l,r)      => Add(simplify(l), simplify(r))    	
    	
    	case Mul(Cst(0),r) => Cst(0)
    	case Mul(l,Cst(0)) => Cst(0)
    	case Mul(Cst(1),r) => simplify(r)
    	case Mul(l,Cst(1)) => simplify(l)
    	case Mul(Mul(a,b),c) => Muls(Set(simplify(a),simplify(b),simplify(c)))
    	case Mul(a, Mul(b,c)) => Muls(Set(simplify(a),simplify(b),simplify(c)))
    	case Mul(Muls(terms1),Muls(terms2)) => Muls(terms1++terms2)
    	case Mul(a,Muls(terms)) => Muls(terms+simplify(a))
    	case Mul(Muls(terms),a) => Muls(terms+simplify(a))
    	case Mul(l,r)      => Mul(simplify(l), simplify(r))
    	
    	case Pow(l,Cst(1)) => simplify(l)
    	case Pow(Cst(0),r) => simplify(r)
    	case Pow(l,r)      => Pow(simplify(l),simplify(r))
    	
    	case Muls(_) => e
    	case Adds(_) => e

    	case _ => e
    }
  }
}

private case class Muls(terms: Set[Expr]) extends Expr
private case class Adds(terms: Set[Expr]) extends Expr

// undefined
case object ? extends Expr



case class Cst(val cst: Int) extends Expr {override def toString() = cst.toString}

case class Var private(id: String, range : Range) extends Expr {
  
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


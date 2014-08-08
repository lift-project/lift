package ir

abstract class FunDef(val params: Array[Param], val isGenerable : Boolean = false) {

  def o(that: Lambda) : CompFunDef = {
    val thisFuns = this match {
      case cf : CompFunDef => cf.funs
      case l : Lambda => Seq(l)
      case _ => Seq(Lambda.FunDefToLambda(this))
    }
    val thatFuns = that match {
      case _ => Seq(that)
    }
    val allFuns = thisFuns ++ thatFuns
    CompFunDef(allFuns:_*)
  }

  def o(that: FunExpr) : FunExpr = {
    apply(that)
  }


  def apply(args : FunExpr*) : FunExpr = {
    assert (args.length == params.length)
    new FunExpr(this, args:_*)
  }

}

class Lambda(override val params: Array[Param], val body: FunExpr) extends FunDef(params, true) {
  override def toString = "Lambda(" + params.map(_.toString).reduce(_ + ", " + _) + "){ " + body.toString + " }"
}

object Lambda {
  implicit def FunDefToLambda(f: FunDef) = {
    new Lambda(f.params, f(f.params:_*))
  }
}

object fun {
  def apply(f: (Param) => FunExpr) = {
    val params = Array(Param(UndefType))
    new Lambda(params, f(params(0)))
  }

  def apply(f: (Param, Param) => FunExpr) = {
    val params = Array(Param(UndefType), Param(UndefType))
    new Lambda(params, f(params(0), params(1)))
  }

  def apply(t: Type, f: (Param) => FunExpr) = {
    val params = Array(Param(t))
    new Lambda(params, f(params(0)))
  }

  def apply(t1: Type, t2: Type, f: (Param, Param) => FunExpr) = {
    val params = Array(Param(t1), Param(t2))
    new Lambda(params, f(params(0), params(1)))
  }

}




object CompFunDef {

  def apply(funs: Lambda*) : CompFunDef = {
     new CompFunDef(funs.last.params,funs:_*)
  }

}

case class CompFunDef(override val params : Array[Param], funs: Lambda*) extends FunDef(params, true) {


  override def toString: String = {
    funs.map((f) => f.toString()).reduce((s1, s2) => s1 + " o " + s2)
  }

  override def equals(o: Any) = {
    o match {
      case cf: CompFunDef => funs.seq.equals(cf.funs)
      case _ => false
    }
  }

  override def hashCode() = {
    funs.foldRight(3*79)((f,hash) => hash*f.hashCode())
  }

  //override def copy() = new CompFunDef(funs.map(f => f.copy()):_*)

  // flatten all the composed functions
  def flatten : List[Lambda] = {
    this.funs.foldLeft(List[Lambda]())((l, f) => {
      f.body.f match {
        case cf: CompFunDef => l ++ cf.flatten
        case _ => l :+ f
      }
    })
  }
}

// Here are just the algorithmic patterns
// For opencl specific patterns see the opencl.ir package

abstract class Pattern(override val params: Array[Param], override val isGenerable: Boolean = false) extends FunDef(params, isGenerable)
/*object Pattern {
  def unapply(p: Pattern) : Option[Context] = Some(p.context)
}*/

trait FPattern {
  def f: Lambda
  //def copy() : FunExpr = this.getClass().getConstructor(classOf[FunExpr]).newInstance(f.copy())
}





abstract class AbstractMap(f:Lambda, override val isGenerable: Boolean = false) extends Pattern(Array[Param](Param(UndefType)), isGenerable) with FPattern
/*object AbstractMap {
  def unapply(am: AbstractMap): Option[FunExpr] = Some(am.f)
}*/

case class Map(f:Lambda) extends AbstractMap(f)


abstract class GenerableMap(f:Lambda) extends AbstractMap(f, true)

abstract class AbstractReduce(f:Lambda, val init: Value, override val isGenerable: Boolean = false)
  extends Pattern(Array[Param](Param(UndefType)), isGenerable) with FPattern

case class Reduce(f: Lambda, override val init: Value) extends AbstractReduce(f, init)

case class PartRed(f: Lambda, init: Value) extends Pattern(Array[Param](Param(UndefType))) with FPattern

case class Join() extends Pattern(Array[Param](Param(UndefType)), true) {
  //override def copy() = Join()
}
case class Split(chunkSize: Expr) extends Pattern(Array[Param](Param(UndefType)), true) {
  //override def copy() = Split(chunkSize)
}

case class asScalar() extends Pattern(Array[Param](Param(UndefType)), true) {
  //override def copy() = asScalar()
}
case class asVector(len: Expr) extends Pattern(Array[Param](Param(UndefType)), true) {
  //override def copy() = asVector(len)
}

/*
// TODO: disuss if this should be a Fun again (if so, this has to be replaced in the very first pass before type checking)
case class Vectorize(n: Expr, f: Fun) extends FPattern {
  def isGenerable() = true
  override def copy() = Vectorize(n, f)
}
*/

object Vectorize {
  class Helper(n: Expr) {
    def apply(uf: UserFunDef): UserFunDef = {
      UserFunDef.vectorize(uf, n)
    }

    def apply(v: Value): Value = {
      Value.vectorize(v, n)
    }
  }

  def apply(n: Expr): Helper = {
    new Helper(n)
  }
}


case class UserFunDef(name: String, paramNames: Any, body: String,
                      inT: Type, outT: Type) extends FunDef(Array[Param](Param(inT)), true) {

  override def toString = "UserFun("+ name + ")" // for debug purposes
}

/*
case class UserFunExpr(val funDef: UserFunDef) extends FunExpr() {
  override def isGenerable() = true
  override def copy() = UserFunExpr(funDef)
}
*/

object UserFunDef {
  def vectorize(uf: UserFunDef, n: Expr): UserFunDef = {
    val name = uf.name + n
    val expectedInT = Type.vectorize(uf.inT, n)
    val expectedOutT = Type.vectorize(uf.outT, n)

    // create new user fun
    UserFunDef(name, uf.paramNames, uf.body, expectedInT, expectedOutT)
  }
}


case class Iterate(n: Expr, f: Lambda) extends Pattern(Array[Param](Param(UndefType)), true) with FPattern {

  override def apply(args: FunExpr*) : IterateExpr = {
    assert(args.length == 1)
    new IterateExpr(this, args(0))
  }

  override def o(that: FunExpr) : IterateExpr = {
    apply(that)
  }
}

object Iterate {
  def apply(n: Expr): ((Lambda) => Iterate)  = (f: Lambda) => Iterate(n ,f)

  def varName(): String = {
    "iterSize"
  }
}

case class Zip() extends FunDef(Array[Param](Param(UndefType),Param(UndefType)), true) {
  //override def copy() = Zip(f1, f2)
}

object Zip {
  def apply(args : FunExpr*) : FunExpr = {
    Zip()(args:_*)
  }
}

/*object Zip {
  def apply(f1: FunExpr): ((FunExpr) => Zip) = (f2: FunExpr) => Zip(f1, f2)
*/

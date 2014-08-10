package ir

abstract class FunDecl(val params: Array[Param]) {

  def isGenerable = false

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

  def o(that: Expr) : FunCall = {
    apply(that)
  }


  def apply(args : Expr*) : FunCall = {
    assert (args.length == params.length)
    new FunCall(this, args:_*)
  }

}

trait isGenerable extends FunDecl {
  override def isGenerable = true
}

object FunDecl {

  def replace(l: Lambda, oldE: Expr, newE: Expr) : Lambda =
    visit (l, (l:Lambda) => {
      if (l.body.eq(oldE)) new Lambda(l.params, newE) else l
    }, (l:Lambda) => l)

  def visit(f: Lambda, pre: (Lambda) => (Lambda), post: (Lambda) => (Lambda)) : Lambda = {

    val newF = pre(f)

    val newBodyFunDef : Expr = newF.body match {
      case call: FunCall => call.f match {
        case l : Lambda => new Lambda( l.params, visit(l, pre, post)(call.args:_*) ).body
        case cfd : CompFunDef => ( new CompFunDef(cfd.params, cfd.funs.map(f => visit(f, pre, post)):_*) )(call.args:_*)
        case ar: AbstractPartRed => ar.getClass.getConstructor(classOf[Lambda],classOf[Value]).newInstance(visit(ar.f, pre, post),ar.init)(call.args:_*)
        case fp: FPattern => fp.getClass.getConstructor(classOf[Lambda]).newInstance(visit(fp.f, pre, post))(call.args:_*)
        case _ => newF.body.copy
      }
      case _ => newF.body.copy
    }

    post(new Lambda(f.params, newBodyFunDef))
  }
}

class Lambda(override val params: Array[Param], val body: Expr) extends FunDecl(params) with isGenerable {
  override def toString = "Lambda(" + params.map(_.toString).reduce(_ + ", " + _) + "){ " + body.toString + " }"
}

object Lambda {
  implicit def FunDefToLambda(f: FunDecl) = {
    new Lambda(f.params, f(f.params:_*))
  }
}

object fun {
  def apply(f: (Param) => Expr) = {
    val params = Array(Param(UndefType))
    new Lambda(params, f(params(0)))
  }

  def apply(f: (Param, Param) => Expr) = {
    val params = Array(Param(UndefType), Param(UndefType))
    new Lambda(params, f(params(0), params(1)))
  }

  def apply(t: Type, f: (Param) => Expr) = {
    val params = Array(Param(t))
    new Lambda(params, f(params(0)))
  }

  def apply(t1: Type, t2: Type, f: (Param, Param) => Expr) = {
    val params = Array(Param(t1), Param(t2))
    new Lambda(params, f(params(0), params(1)))
  }

}




object CompFunDef {

  def apply(funs: Lambda*) : CompFunDef = {
     new CompFunDef(funs.last.params,funs:_*)
  }

}

case class CompFunDef(override val params : Array[Param], funs: Lambda*) extends FunDecl(params)  with isGenerable {


  override def toString: String = {
    funs.map((f) => f.toString()).reduce((s1, s2) => s1 + " o " + s2)
  }

  override def equals(o: Any) = {
    o match {
      case cf : CompFunDef => funs.seq.equals(cf.funs)
      case _ => false
    }
  }

  override def hashCode() = {
    funs.foldRight(3*79)((f,hash) => hash*f.hashCode())
  }

  //override def copy() = new CompFunDef(funs.map(f => f.copy()):_*)

  // flatten all the composed functions
  def flatten : List[Lambda] = {
    this.funs.foldLeft(List[Lambda]())((ll, f) => {
      f.body match {
        case call: FunCall => call.f match {
            case cf: CompFunDef => ll ++ cf.flatten
            case _ => ll :+ f
          }
        case _ => ll :+ f
      }
    })
  }
}

// Here are just the algorithmic patterns
// For opencl specific patterns see the opencl.ir package

abstract class Pattern(override val params: Array[Param]) extends FunDecl(params)
/*object Pattern {
  def unapply(p: Pattern) : Option[Context] = Some(p.context)
}*/

trait FPattern {
  def f: Lambda
  //def copy() : FunExpr = this.getClass().getConstructor(classOf[FunExpr]).newInstance(f.copy())
}





abstract class AbstractMap(f:Lambda) extends Pattern(Array[Param](Param(UndefType))) with FPattern
/*object AbstractMap {
  def unapply(am: AbstractMap): Option[FunExpr] = Some(am.f)
}*/

case class Map(f:Lambda) extends AbstractMap(f)


abstract class GenerableMap(f:Lambda) extends AbstractMap(f) with isGenerable

abstract class AbstractPartRed(f:Lambda, val init: Value) extends Pattern(Array[Param](Param(UndefType))) with FPattern

abstract class AbstractReduce(f:Lambda, override val init: Value) extends AbstractPartRed(f, init)

case class Reduce(f: Lambda, override val init: Value) extends AbstractReduce(f, init)

case class PartRed(f: Lambda, override val init: Value) extends AbstractPartRed(f,init) with FPattern

case class Join() extends Pattern(Array[Param](Param(UndefType))) with isGenerable {
  //override def copy() = Join()
}
case class Split(chunkSize: ArithExpr) extends Pattern(Array[Param](Param(UndefType))) with isGenerable {
  //override def copy() = Split(chunkSize)
}

case class asScalar() extends Pattern(Array[Param](Param(UndefType))) with isGenerable {
  //override def copy() = asScalar()
}
case class asVector(len: ArithExpr) extends Pattern(Array[Param](Param(UndefType))) with isGenerable {
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
  class Helper(n: ArithExpr) {
    def apply(uf: UserFunDef): UserFunDef = {
      UserFunDef.vectorize(uf, n)
    }

    def apply(v: Value): Value = {
      Value.vectorize(v, n)
    }
  }

  def apply(n: ArithExpr): Helper = {
    new Helper(n)
  }
}


case class UserFunDef(name: String, paramNames: Any, body: String,
                      inT: Type, outT: Type) extends FunDecl(Array[Param](Param(inT))) with isGenerable {

  override def toString = "UserFun("+ name + ")" // for debug purposes
}

/*
case class UserFunExpr(val funDef: UserFunDef) extends FunExpr() {
  override def isGenerable() = true
  override def copy() = UserFunExpr(funDef)
}
*/

object UserFunDef {
  def vectorize(uf: UserFunDef, n: ArithExpr): UserFunDef = {
    val name = uf.name + n
    val expectedInT = Type.vectorize(uf.inT, n)
    val expectedOutT = Type.vectorize(uf.outT, n)

    // create new user fun
    UserFunDef(name, uf.paramNames, uf.body, expectedInT, expectedOutT)
  }
}


case class Iterate(n: ArithExpr, f: Lambda) extends Pattern(Array[Param](Param(UndefType))) with FPattern with isGenerable {

  override def apply(args: Expr*) : IterateCall = {
    assert(args.length == 1)
    new IterateCall(this, args(0))
  }

  override def o(that: Expr) : IterateCall = {
    apply(that)
  }
}

object Iterate {
  def apply(n: ArithExpr): ((Lambda) => Iterate)  = (f: Lambda) => Iterate(n ,f)

  def varName(): String = {
    "iterSize"
  }
}

case class Zip() extends FunDecl(Array[Param](Param(UndefType),Param(UndefType))) with isGenerable {
  //override def copy() = Zip(f1, f2)
}

object Zip {
  def apply(args : Expr*) : FunCall = {
    assert(args.length == 2)
    Zip()(args:_*)
  }
}

/*object Zip {
  def apply(f1: FunExpr): ((FunExpr) => Zip) = (f2: FunExpr) => Zip(f1, f2)
*/

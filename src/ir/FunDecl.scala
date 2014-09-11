package ir

import java.util.function._
import scala.collection.JavaConverters._

abstract class FunDecl(val params: Array[Param]) {

  def isGenerable = false

  def comp(that: Lambda) : CompFunDef = {
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
  def comp(f: FunDecl): CompFunDef = comp(Lambda.FunDefToLambda(f))

  def o(f: Lambda): CompFunDef = comp(f)

  def call(arg: Expr) = apply(arg)
  def call(arg0: Expr, arg1: Expr) = apply(arg0, arg1)
  def call(arg0: Expr, arg1: Expr, arg2: Expr) = apply(arg0, arg1, arg2)

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
  implicit def FunDefToLambda(f: FunDecl): Lambda = {
    val params = f.params.map(_ => Param(UndefType))
    new Lambda(params, f(params:_*))
  }
}

class Lambda1(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 1)
}

object Lambda1 {
  implicit def FunDefToLambda(f: FunDecl) = {
    assert(f.params.nonEmpty)
    if (f.params.length == 1) {
      fun(f(_))
    } else {
      fun( x => f( f.params.zipWithIndex.map({ case (_,i) => Get(x, i) }):_* ) )
    }
  }
}

class Lambda2(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 2)

  def apply(arg: Expr): Lambda1 = {
    fun( tmp => super.apply(arg, tmp) )
  }
}

object Lambda2 {
  implicit def FunDefToLambda(f: FunDecl): Lambda2 = {
    assert(f.params.length == 2)
    fun(f(_, _))
  }
}

class Lambda3(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 3)

  def apply(arg0: Expr, arg1: Expr): Lambda1 = {
    fun( tmp => super.apply(arg0, arg1, tmp) )
  }

  def apply(arg: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg, tmp0, tmp1) )
  }
}

class Lambda4(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 4)

  def apply(arg0: Expr, arg1: Expr, arg2: Expr): Lambda1 = {
    fun( tmp => super.apply(arg0, arg1, arg2, tmp) )
  }

  def apply(arg0: Expr, arg1: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg0, arg1, tmp0, tmp1) )
  }

  def apply(arg: Expr): Lambda3 = {
    fun( (tmp0, tmp1, tmp2) => super.apply(arg, tmp0, tmp1, tmp2) )
  }
}

class Lambda5(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 5)

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Lambda1 = {
    fun( tmp => super.apply(arg0, arg1, arg2, arg3, tmp) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg0, arg1, arg2, tmp0, tmp1) )
  }

  def apply(arg0: Expr, arg1: Expr): Lambda3 = {
    fun( (tmp0, tmp1, tmp2) => super.apply(arg0, arg1, tmp0, tmp1, tmp2) )
  }

  def apply(arg: Expr): Lambda4 = {
    fun( (tmp0, tmp1, tmp2, tmp3) => super.apply(arg, tmp0, tmp1, tmp2, tmp3) )
  }
}

object jfun {
  // create lambda1
  def create(f: Function[Param, Expr]): Lambda1 = {
    val params = Array(Param(UndefType))
    new Lambda1(params, f.apply(params(0)))
  }

  def create(t: Type, f: Function[Param, Expr]): Lambda1 = {
    val params = Array(Param(t))
    new Lambda1(params, f.apply(params(0)))
  }

  // create lambda2
  def create(f: BiFunction[Param, Param, Expr]): Lambda2 = {
    val params = Array(Param(UndefType), Param(UndefType))
    new Lambda2(params, f.apply(params(0), params(1)))
  }

  def create(t1: Type, t2: Type, f: BiFunction[Param, Param, Expr]): Lambda2 = {
    val params = Array(Param(t1), Param(t2))
    new Lambda2(params, f.apply(params(0), params(1)))
  }

  //create lambda3
  def create(f: TriFunction[Param, Param, Param, Expr]): Lambda3 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda3(params, f.apply(params(0), params(1), params(2)))
  }

  def create(t1: Type, t2: Type, t3: Type, f: TriFunction[Param, Param, Param, Expr]): Lambda3 = {
    val params = Array(Param(t1), Param(t2), Param(t3))
    new Lambda3(params, f.apply(params(0), params(1), params(2)))
  }
}

object fun {
  def apply(f: (Param) => Expr): Lambda1 = {
    val params = Array(Param(UndefType))
    new Lambda1(params, f(params(0)))
  }

  def apply(f: (Param, Param) => Expr): Lambda2 = {
    val params = Array(Param(UndefType), Param(UndefType))
    new Lambda2(params, f(params(0), params(1)))
  }

  def apply(f: (Param, Param, Param) => Expr): Lambda3 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda3(params, f(params(0), params(1), params(2)))
  }

  def apply(f: (Param, Param, Param, Param) => Expr): Lambda4 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda4(params, f(params(0), params(1), params(2), params(3)))
  }

  def apply(f: (Param, Param, Param, Param, Param) => Expr): Lambda5 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda5(params, f(params(0), params(1), params(2), params(3), params(4)))
  }

  def apply(t: Type, f: (Param) => Expr): Lambda1 = {
    val params = Array(Param(t))
    new Lambda1(params, f(params(0)))
  }

  def apply(t1: Type, t2: Type, f: (Param, Param) => Expr): Lambda2 = {
    val params = Array(Param(t1), Param(t2))
    new Lambda2(params, f(params(0), params(1)))
  }

  def apply(t1: Type, t2: Type, t3: Type, f: (Param, Param, Param) => Expr): Lambda3 = {
    val params = Array(Param(t1), Param(t2), Param(t3))
    new Lambda3(params, f(params(0), params(1), params(2)))
  }

  def apply(t1: Type, t2: Type, t3: Type, t4: Type, f: (Param, Param, Param, Param) => Expr): Lambda4 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4))
    new Lambda4(params, f(params(0), params(1), params(2), params(3)))
  }

  def apply(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, f: (Param, Param, Param, Param, Param) => Expr): Lambda5 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5))
    new Lambda5(params, f(params(0), params(1), params(2), params(3), params(4)))
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





abstract class AbstractMap(f:Lambda1) extends Pattern(Array[Param](Param(UndefType))) with FPattern
/*object AbstractMap {
  def unapply(am: AbstractMap): Option[FunExpr] = Some(am.f)
}*/

case class Map(f:Lambda1) extends AbstractMap(f) {
  override def apply(args: Expr*): MapCall = createMapCall(args:_*)

  override def o(that: Expr): MapCall = createMapCall(that)

  private def createMapCall(args: Expr*): MapCall = {
    assert(args.length == 1)
    new MapCall("Map", Var(""), this, args(0))
  }
}

object Map {
  def apply(f: Lambda1, expr: Expr): MapCall = {
    Map(f).createMapCall(expr)
  }
}

object jMap {
  def create(f: Lambda1) = Map(f)
  def create(f: FunDecl) = Map(Lambda1.FunDefToLambda(f))
}


abstract class GenerableMap(f:Lambda1) extends AbstractMap(f) with isGenerable

abstract class AbstractPartRed(f:Lambda2) extends Pattern(Array[Param](Param(UndefType), Param(UndefType))) with FPattern {
  def init: Value = params(0) match { case v: Value => v}
}

abstract class AbstractReduce(f:Lambda2) extends AbstractPartRed(f)

case class Reduce(f: Lambda2) extends AbstractReduce(f) {
  override def apply(args: Expr*) : ReduceCall = reduceCall(args:_*)

  override def o(that: Expr) : ReduceCall =  reduceCall(that)

  private def reduceCall(args: Expr*): ReduceCall = {
    assert(args.length == 2)
    new ReduceCall(Var("i"), this, args(0), args(1))
  }
}
object Reduce {
  def apply(f: Lambda2, init: Value): Lambda = fun((x) => Reduce(f)(init, x))
}

object jReduce {
  def create(f: Lambda2, init: Value) = Reduce(f, init)
  def create(f: FunDecl, init: Value) = Reduce(Lambda2.FunDefToLambda(f), init)
}

case class PartRed(f: Lambda2) extends AbstractPartRed(f) with FPattern {
  override def apply(args: Expr*) : ReduceCall = reduceCall(args:_*)

  override def o(that: Expr) : ReduceCall = reduceCall(that)

  private def reduceCall(args: Expr*): ReduceCall = {
    assert(args.length == 2)
    new ReduceCall(Var("i"), this, args(0), args(1))
  }
}
object PartRed {
  def apply(f: Lambda2, init: Value): Lambda = fun((x) => PartRed(f)(init, x))
}

case class Join() extends Pattern(Array[Param](Param(UndefType))) with isGenerable {
  //override def copy() = Join()
}
object jJoin {
  def create = Join()

  def comp(f: Lambda): CompFunDef = create o f
  def comp(f: FunDecl): CompFunDef = create o Lambda.FunDefToLambda(f)
}

case class JoinDim2() extends  Pattern(Array[Param](Param(UndefType))) with isGenerable

case class Split(chunkSize: ArithExpr) extends Pattern(Array[Param](Param(UndefType))) with isGenerable {
  //override def copy() = Split(chunkSize)
}
object jSplit {
  def create(c: Int) = Split(c)
}

case class SplitDim2(chunkSize: ArithExpr) extends Pattern(Array[Param](Param(UndefType))) with isGenerable

case class asScalar() extends Pattern(Array[Param](Param(UndefType))) with isGenerable {
  //override def copy() = asScalar()
}
object jAsScalar {
  def create = asScalar()

  def comp(f: Lambda) = create o f
  def comp(f: FunDecl) = create o Lambda.FunDefToLambda(f)
}

case class asVector(len: ArithExpr) extends Pattern(Array[Param](Param(UndefType))) with isGenerable {
  //override def copy() = asVector(len)
}
object jAsVector {
  def create(c: Int) = asVector(c)
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
                      inT: Type, outT: Type)
//  extends FunDecl(UserFunDef.toParams(inT)) with isGenerable {
  extends FunDecl( inT match {
      case tt: TupleType => tt.elemsT.map(Param(_)).toArray
      case t: Type => Array(Param(t))
    } ) with isGenerable {

  override def toString = "UserFun("+ name + ")" // for debug purposes
}

object UserFunDef {
  def vectorize(uf: UserFunDef, n: ArithExpr): UserFunDef = {
    val name = uf.name + n
    val expectedInT = Type.vectorize(uf.inT, n)
    val expectedOutT = Type.vectorize(uf.outT, n)

    // create new user fun
    UserFunDef(name, uf.paramNames, uf.body, expectedInT, expectedOutT)
  }

  //private def toParams(t: Type): Array[Param] = {
  //  t match {
  //    case tt: TupleType => tt.elemsT.map(toParams).flatten.toArray
  //    case t: Type => Array(Param(t))
  //  }
  //}
}


case class Iterate(n: ArithExpr, f: Lambda1) extends Pattern(Array[Param](Param(UndefType))) with FPattern with isGenerable {

  override def apply(args: Expr*) : IterateCall = iterateCall(args:_*)

  override def o(that: Expr) : IterateCall = iterateCall(that)

  private def iterateCall(args: Expr*): IterateCall = {
    assert(args.length == 1)
    new IterateCall(this, args(0))
  }
}

object Iterate {
  def apply(n: ArithExpr): ((Lambda1) => Iterate)  = (f: Lambda1) => Iterate(n ,f)

  def varName(): String = {
    "iterSize"
  }
}

object jIterate {
  def create(n: Int, f: Lambda1) = Iterate(n, f)
  def create(n: ArithExpr, f: Lambda1) = Iterate(n, f)

  def create(n: Int, f: FunDecl) = Iterate(n, Lambda1.FunDefToLambda(f))
  def create(n: ArithExpr, f: FunDecl) = Iterate(n, Lambda1.FunDefToLambda(f))
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

object jZip {
  def create = Zip()

  def call(arg0: Expr, arg1: Expr) = Zip()(arg0, arg1)

  def call(args: java.util.List[Expr]) = Zip()(args.asScala:_*)
}

case class Unzip() extends FunDecl(Array[Param](Param(UndefType))) with isGenerable

object Unzip {
  def apply(args: Expr*): FunCall = {
    assert(args.length == 2)
    Unzip()(args:_*)
  }
}

/*object Zip {
  def apply(f1: FunExpr): ((FunExpr) => Zip) = (f2: FunExpr) => Zip(f1, f2)
*/

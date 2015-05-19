package ir.interop

import arithmetic.ArithExpr
import ir._
import java.util.function._
import scala.collection.JavaConverters._

object jTypeArray {
  type T = Type
  def create(t: T) = Array(t)
  def create(t0: T, t1: T) = Array(t0, t1)
  def create(t0: T, t1: T, t2: T) = Array(t0, t1, t2)
  def create(t0: T, t1: T, t2: T, t3: T) = Array(t0, t1, t2, t3)
  def create(t0: T, t1: T, t2: T, t3: T, t4: T) = Array(t0, t1, t2, t3, t4)
  def create(t0: T, t1: T, t2: T, t3: T, t4: T, t5: T) = Array(t0, t1, t2, t3, t4, t5)
  def create(t0: T, t1: T, t2: T, t3: T, t4: T, t5: T, t6: T) = Array(t0, t1, t2, t3, t4, t5, t6)
  def create(t0: T, t1: T, t2: T, t3: T, t4: T, t5: T, t6: T, t7: T) = Array(t0, t1, t2, t3, t4, t5, t6, t7)
  def create(t0: T, t1: T, t2: T, t3: T, t4: T, t5: T, t6: T, t7: T, t8: T) = Array(t0, t1, t2, t3, t4, t5, t6, t7, t8)
  def create(t0: T, t1: T, t2: T, t3: T, t4: T, t5: T, t6: T, t7: T, t8: T, t9: T) = Array(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)
}

object jStringArray {
  type T = String
  def create(t: T) = Array(t)
  def create(t0: T, t1: T) = Array(t0, t1)
  def create(t0: T, t1: T, t2: T) = Array(t0, t1, t2)
  def create(t0: T, t1: T, t2: T, t3: T) = Array(t0, t1, t2, t3)
  def create(t0: T, t1: T, t2: T, t3: T, t4: T) = Array(t0, t1, t2, t3, t4)
  def create(t0: T, t1: T, t2: T, t3: T, t4: T, t5: T) = Array(t0, t1, t2, t3, t4, t5)
  def create(t0: T, t1: T, t2: T, t3: T, t4: T, t5: T, t6: T) = Array(t0, t1, t2, t3, t4, t5, t6)
  def create(t0: T, t1: T, t2: T, t3: T, t4: T, t5: T, t6: T, t7: T) = Array(t0, t1, t2, t3, t4, t5, t6, t7)
  def create(t0: T, t1: T, t2: T, t3: T, t4: T, t5: T, t6: T, t7: T, t8: T) = Array(t0, t1, t2, t3, t4, t5, t6, t7, t8)
  def create(t0: T, t1: T, t2: T, t3: T, t4: T, t5: T, t6: T, t7: T, t8: T, t9: T) = Array(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)
}

object jArrayType {
  def create(elemT: Type, len: ArithExpr) = ArrayType(elemT, len)
}

object jTupleType {
  def create(elemT0: Type, elemT1: Type) = TupleType(elemT0, elemT1)
  def create(elemT0: Type, elemT1: Type, elemT2: Type) = TupleType(elemT0, elemT1, elemT2)
  def create(elemT0: Type, elemT1: Type, elemT2: Type, elemT3: Type) = TupleType(elemT0, elemT1, elemT2, elemT3)

  def create(elemTs: java.util.List[Type]) = TupleType(elemTs.asScala:_*)
}

object jVectorType {
  def create(scalarT: ScalarType, len: ArithExpr) = VectorType(scalarT, len)
}

object jScalarType {
  def create(name: String, size: ArithExpr) = ScalarType(name, size)
}

object jAsScalar {
  def create = asScalar()

  def comp(f: Lambda) = create o f
  def comp(f: FunDecl) = create o Lambda.FunDefToLambda(f)
}

object jAsVector {
  def create(c: Int) = asVector(c)
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

  //create lambda4
  def create(f: QuadFunction[Param, Param, Param, Param, Expr]): Lambda4 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda4(params, f.apply(params(0), params(1), params(2), params(3)))
  }

  def create(t1: Type, t2: Type, t3: Type, t4: Type, f: QuadFunction[Param, Param, Param, Param, Expr]): Lambda4 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4))
    new Lambda4(params, f.apply(params(0), params(1), params(2), params(3)))
  }

  //create lambda5
  def create(f: QuintFunction[Param, Param, Param, Param, Param, Expr]): Lambda5 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda5(params, f.apply(params(0), params(1), params(2), params(3), params(4)))
  }

  def create(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, f: QuintFunction[Param, Param, Param, Param, Param, Expr]): Lambda5 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5))
    new Lambda5(params, f.apply(params(0), params(1), params(2), params(3), params(4)))
  }

  //create lambda6
  def create(f: HexaFunction[Param, Param, Param, Param, Param, Param, Expr]): Lambda6 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda6(params, f.apply(params(0), params(1), params(2), params(3), params(4), params(5)))
  }

  def create(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, f: HexaFunction[Param, Param, Param, Param, Param, Param, Expr]): Lambda6 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5), Param(t6))
    new Lambda6(params, f.apply(params(0), params(1), params(2), params(3), params(4), params(5)))
  }

  //create lambda7
  def create(f: SeptFunction[Param, Param, Param, Param, Param, Param, Param, Expr]): Lambda7 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda7(params, f.apply(params(0), params(1), params(2), params(3), params(4), params(5), params(6)))
  }

  def create(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, f: SeptFunction[Param, Param, Param, Param, Param, Param, Param, Expr]): Lambda7 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5), Param(t6), Param(t7))
    new Lambda7(params, f.apply(params(0), params(1), params(2), params(3), params(4), params(5), params(6)))
  }

  //create lambda8
  def create(f: OctoFunction[Param, Param, Param, Param, Param, Param, Param, Param, Expr]): Lambda8 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda8(params, f.apply(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7)))
  }

  def create(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, f: OctoFunction[Param, Param, Param, Param, Param, Param, Param, Param, Expr]): Lambda8 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5), Param(t6), Param(t7), Param(t8))
    new Lambda8(params, f.apply(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7)))
  }

  //create lambda9
  def create(f: NovemFunction[Param, Param, Param, Param, Param, Param, Param, Param, Param, Expr]): Lambda9 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda9(params, f.apply(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8)))
  }

  def create(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9: Type, f: NovemFunction[Param, Param, Param, Param, Param, Param, Param, Param, Param, Expr]): Lambda9 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5), Param(t6), Param(t7), Param(t8), Param(t9))
    new Lambda9(params, f.apply(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8)))
  }
}

object jIterate {
  def create(n: Int, f: Lambda1) = Iterate(n, f)
  def create(n: ArithExpr, f: Lambda1) = Iterate(n, f)

  def create(n: Int, f: FunDecl) = Iterate(n, Lambda1.FunDefToLambda(f))
  def create(n: ArithExpr, f: FunDecl) = Iterate(n, Lambda1.FunDefToLambda(f))
}

object jJoin {
  def create = Join()

  def comp(f: Lambda): CompFunDef = create o f
  def comp(f: FunDecl): CompFunDef = create o Lambda.FunDefToLambda(f)
}

object jMap {
  def create(f: Lambda1) = Map(f)
  def create(f: FunDecl) = Map(Lambda1.FunDefToLambda(f))
}

object jReduce {
  def create(f: Lambda2, init: Value) = Reduce(f, init)
  def create(f: FunDecl, init: Value) = Reduce(Lambda2.FunDefToLambda(f), init)
}

object jSplit {
  def create(c: Int) = Split(c)
}

object jUserFunDef {
  def create(name: String, paramName: String, body: String, inT: Type, outT: Type): UserFunDef = {
    UserFunDef(name, paramName, body, inT, outT)
  }

  def create(name: String, paramNames: Array[String], body: String, inTs: Array[Type], outT: Type): UserFunDef = {
    UserFunDef(name, paramNames, body, inTs, outT)
  }
}

object jZip {
  def create = Zip()

  def call(arg0: Expr, arg1: Expr) = Zip(arg0, arg1)

  def call(args: java.util.List[Expr]) = Zip(args.asScala:_*)
}

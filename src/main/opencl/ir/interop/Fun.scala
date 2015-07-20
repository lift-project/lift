package opencl.ir.interop

import java.util.function.BiFunction

import arithmetic.ArithExpr
import ir._
import ir.ast._
import opencl.ir.pattern._

object jGather {

  def create(idx: BiFunction[ArithExpr, Type, ArithExpr]) = {
    val idxLambda = (a: ArithExpr, t: Type) => idx(a,t)
    Gather(idxLambda)
  }
}

object jScatter {
  def create(idx: BiFunction[ArithExpr, Type, ArithExpr]) = {
    val idxLambda = (a: ArithExpr, t: Type) => idx(a,t)
    Scatter(idxLambda)
  }
}

object jMapGlb {
  def create(f: Lambda1) = MapGlb(f)
  def create(f: FunDecl) = MapGlb(Lambda1.FunDefToLambda(f))
}

object jMapWrg {
  def create(f: Lambda1) = MapWrg(f)
  def create(f: FunDecl) = MapWrg(Lambda1.FunDefToLambda(f))
}

object jMapLcl {
  def create(f: Lambda1) = MapLcl(f)
  def create(f: FunDecl) = MapLcl(Lambda1.FunDefToLambda(f))
}

object jMapWarp {
  def create(f: Lambda1) = MapWarp(f)
  def create(f: FunDecl) = MapWarp(Lambda1.FunDefToLambda(f))
}

object jMapLane {
  def create(f: Lambda1) = MapLane(f)
  def create(f: FunDecl) = MapLane(Lambda1.FunDefToLambda(f))
}

object jMapSeq {
  def create(f: Lambda1) = MapSeq(f)
  def create(f: FunDecl) = MapSeq(Lambda1.FunDefToLambda(f))
}

object jReduceHost {
  def create(f: Lambda2, init: Value) = ReduceHost(f, init)
  def create(f: FunDecl, init: Value) = ReduceHost(Lambda1.FunDefToLambda(f), init)
}

object jReduceSeq {
  def create(f: Lambda2, init: Value) = ReduceSeq(f, init)
  def create(f: FunDecl, init: Value) = ReduceSeq(Lambda2.FunDefToLambda(f), init)
}

object jReorderStride {
  def create (s: ArithExpr)= ReorderStride(s)
}

object jTranspose {
  def create = Transpose()

  def comp(f: Lambda) = create o f
  def comp(f: FunDecl) = create o Lambda.FunDefToLambda(f)
}

object jToGlobal {
  def create(f: Lambda1) = toGlobal(f)
  def create(f: FunDecl) = toGlobal(Lambda1.FunDefToLambda(f))
}

object jToLocal {
  def create(f: Lambda1) = toLocal(f)
  def create(f: FunDecl) = toLocal(Lambda1.FunDefToLambda(f))
}

object jGroup {
  def create(relIndices: Array[Int], negOOB: (ArithExpr, ArithExpr) => ArithExpr,
             posOOB: (ArithExpr, ArithExpr) => ArithExpr) = Group(relIndices, negOOB, posOOB)
}

object jGroup2D {
  def create(relColumns: Array[Int], relRows: Array[Int], negOOB: (ArithExpr, ArithExpr) => ArithExpr,
             posOOB: (ArithExpr, ArithExpr) => ArithExpr) = Group2D(relColumns, relRows, negOOB, posOOB)
}

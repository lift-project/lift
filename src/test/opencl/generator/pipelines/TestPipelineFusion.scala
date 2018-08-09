package opencl.generator.pipelines

import ir._
import ir.ast._
import ir.ast.debug.PrintType
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit._

object TestPipelineFusion extends TestWithExecutor

class TestPipelineFusion {

  @Test
  def boxBlurPipelineFusion(): Unit = {
    val M = SizeVar("M")
    val N = SizeVar("N")

    def lambda(l: Lambda): Lambda = {
      λ(ArrayType(ArrayType(Float, N), M),
      image => l $ image
              )
    }

    val blurx =
      Map(λ(row => Join() o
        Map(λ(window =>
          Reduce(add, 0.0f) o
          Map(id) $ window) // some meaningful userfun
        ) o Slide(3,1) $ row))

    val blury = Transpose() o blurx o Transpose()

    val pipeline = lambda(blury o blurx)

    val f0 = PrintType() o Transpose() o blurx o Transpose() o // blury
      Map(Join() o Map(Reduce(add, 0.0f) o Map(id)) o Slide(3,1)) // blurx

    val f1 = PrintType() o Transpose() o blurx o
      Transpose() o
      Map(Join()) o
      Map(Map(Reduce(add, 0.0f))) o
      Map(Map(Map(id))) o
      Map(Slide(3,1))

    val f2 = PrintType() o Transpose() o blurx o
      Join() o
      Map(Transpose()) o
      Transpose() o
      Map(Map(Reduce(add, 0.0f))) o
      Map(Map(Map(id))) o
      Map(Slide(3,1))

    val f3 = PrintType() o Transpose() o blurx o
      Join() o
      Map(Transpose()) o
      Map(Map(Reduce(add, 0.0f))) o
      Transpose() o
      Map(Map(Map(id))) o
      Map(Slide(3,1))

    val f4 = PrintType() o Transpose() o blurx o
      Join() o
      Map(Transpose()) o
      Map(Map(Reduce(add, 0.0f))) o
      Map(Map(Map(id))) o
      Transpose() o
      Map(Slide(3,1))

    // high-level
    val P = PrintType()
    val T = Transpose()
    val J = Join()
    val S = Slide(3,1)
    val R = Reduce(add, 0.0f)
    val f = id
    def *(f: Lambda) = Map(f)
    def **(f: Lambda) = Map(Map(f))
    def ***(f: Lambda) = Map(Map(Map((f))))
    def ****(f: Lambda) = Map(Map(Map(Map((f)))))

    // low-level
    def %(f: Lambda) = MapSeq(f)
    def %%(f: Lambda) = MapSeq(MapSeq(f))
    val RS = ReduceSeq(add, 0.0f)
    val TW = TransposeW()

    val f5 = P o T o *(J) o **(R) o ***(f) o J o **(S) o *(T) o **(R) o ***(f) o T o *(S)
    val f6 = P o T o *(J) o **(R) o J o ****(f) o **(S) o *(T) o **(R) o ***(f) o T o *(S)
    val f7 = P o T o *(J) o J o ***(R) o ****(f) o **(S) o *(T) o **(R) o ***(f) o T o *(S)

    // parallelizing f7
    val p0 = P o T o *(J) o J o *(**(R) o ***(f) o *(S) o T o *(R) o **(f)) o T o *(S)
    val p1 = P o T o *(J) o J o *(*(*(R) o **(f) o (S)) o T o *((R) o *(f))) o T o *(S)
    val p2 = P o TransposeW() o *(J) o J o MapGlb(%(%(%(toGlobal(id)) o ReduceSeq(add, 0.0f)) o %(%(f)) o (S)) o T o %((%(toGlobal(id)) o ReduceSeq(add, 0.0f)) o %(f))) o T o *(S)

    // moving slide forward
    val f8 = P o T o *(J) o J o ***(R) o ****(f) o *(T) o **(T) o *(S) o **(R) o ***(f) o T o *(S)
    val f9 = P o T o *(J) o J o ***(R) o ****(f) o *(T) o **(T) o ***(R) o *(S) o ***(f) o T o *(S)
    val f10 = P o T o *(J) o J o ***(R) o ****(f) o *(T) o **(T) o ***(R) o ****(f) o *(S) o T o *(S)
    val f11 = P o T o *(J) o J o ***(R) o ****(f) o P o *(T) o **(T) o P o ***(R) o ****(f) o T o *(T) o S o *(S)
    val f12 = P o T o *(J) o J o *(T) o ***(R) o ****(f) o **(T) o ***(R) o ****(f) o T o *(T) o S o *(S)

    // parallelizing f12
    val g0 = P o T o *(J) o J o *(T) o **(*(R) o **(f) o T o *(R) o **(f)) o T o *(T) o S o *(S)

    // lowering g0
    // process 3x3 using workgroup and one thread computes blury
    val g1 =     TW o *(J) o J o *(TW) o
      MapWrg(1)(MapWrg(0)(
        MapLcl(
          MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f) //o MapSeq(f)
        ) o Transpose() o // synchronization here!
        MapLcl(
          MapSeq(toLocal(id)) o ReduceSeq(add, 0.0f) //o MapSeq(f)
        )
      )) o T o *(T) o S o *(S)

    // process 3x3 by single thread, reuse of blurx in private memory
    val g2 =     TW o *(J) o J o *(TW) o
      MapGlb(1)(MapGlb(0)(
        MapSeq(
          MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f) //o MapSeq(f)
        ) o Transpose() o // synchronization here!
        MapSeq(
          MapSeq(toPrivate(id)) o ReduceSeq(add, 0.0f) //o MapSeq(f)
        )
      )) o T o *(T) o S o *(S)

    val input = Array.tabulate(32, 32) { (i, j) => i * 32.0f + j }
    val (outG1, _) = Execute(1,1,32,32,(false,false))[Array[Float]](lambda(g1), input)
    val (outG2, _) = Execute(1,1,32,32,(false,false))[Array[Float]](lambda(g2), input)
    assertArrayEquals(outG1, outG2, 0.1f)

    TypeChecker(lambda(f0))
    TypeChecker(lambda(f1))
    TypeChecker(lambda(f2))
    TypeChecker(lambda(f3))
    TypeChecker(lambda(f4))
    TypeChecker(lambda(f5))
    TypeChecker(lambda(f6))
    TypeChecker(lambda(p0))
    TypeChecker(lambda(p1))
    TypeChecker(lambda(p2))
    TypeChecker(lambda(f8))
    TypeChecker(lambda(f9))
    TypeChecker(lambda(f10))
    TypeChecker(lambda(f11))
    TypeChecker(lambda(f12))
    TypeChecker(lambda(g0))
    TypeChecker(lambda(g1))
    TypeChecker(lambda(g2))
  }
}

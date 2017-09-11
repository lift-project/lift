package rewriting

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.Compile
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Test

class TestRewriteMriQ {

  @Test
  def computeQ(): Unit = {
    val x = SizeVar("X")
    val k = SizeVar("K")

    // TODO: Break into ReduceSeq o Map? or Map() o Reduce() o Map
    // Similar pattern to kmeans or gemv/nbody
    val qFun = UserFun("computeQ",
      Array("sX", "sY", "sZ", "Kx", "Ky", "Kz", "PhiMag", "acc"),
      """{
        |    #define PIx2 6.2831853071795864769252867665590058f
        |    float expArg = PIx2 * (Kx * sX + Ky * sY + Kz * sZ);
        |    acc._0 = acc._0 + PhiMag * cos(expArg);
        |    acc._1 = acc._1 + PhiMag * sin(expArg);
        |
        |    return acc;
        |}""".stripMargin,
      Seq(Float, Float, Float, Float, Float, Float, Float, TupleType(Float, Float)),
      TupleType(Float, Float))

    val pair = UserFun("pair", Array("x", "y"), "{ Tuple t = {x, y}; return t; }",
      Seq(Float, Float), TupleType(Float, Float))

    val f = fun(
      ArrayType(Float, x),
      ArrayType(Float, x),
      ArrayType(Float, x),
      ArrayType(Float, x),
      ArrayType(Float, x),
      ArrayType(TupleType(Float, Float, Float, Float), k),
      (x, y, z, Qr, Qi, kvalues) =>
        Map(\(t =>
          ReduceSeq(\((acc, p) =>
            qFun(t._0, t._1, t._2, p._0, p._1, p._2, p._3, acc)
          ), Tuple(t._3, t._4)) $ kvalues
        )) $ Zip(x, y, z, Qr, Qi)
    )

    val f0 = Rewrite.applyRuleAtId(f, 0, Rules.addIdForMapParam)
    val f1 = Rewrite.applyRuleAtId(f0, 8, Rules.implementOneLevelOfId)
    val f2 = Rewrite.applyRuleAtId(f1, 21, Rules.implementIdAsDeepCopy)
    val f3 = Rewrite.applyRuleAtId(f2, 18, Rules.implementIdAsDeepCopy)
    val f4 = Rewrite.applyRuleAtId(f3, 15, Rules.implementIdAsDeepCopy)
    val f5 = Rewrite.applyRuleAtId(f4, 12, Rules.dropId)
    val f6 = Rewrite.applyRuleAtId(f5, 9, Rules.dropId)

    val f7 = Rewrite.applyRuleAtId(f6, 24, Rules.tupleToStruct)
    val f8 = Lower.lowerNextLevelWithRule(f7, Rules.mapGlb)
    val f9 = Rewrite.applyRuleAtId(f8, 22, Rules.addIdAfterReduce)
    val f10 = Rewrite.applyRuleAtId(f9, 22, Rules.globalMemory)
    val f11 = Rewrite.applyRuleAtId(f10, 49, Rules.implementIdAsDeepCopy)
    val f12 = Rewrite.applyRuleAtId(f11, 25, Rules.privateMemory)
    val f13 = Rewrite.applyRuleAtId(f12, 19, Rules.privateMemory)
    val f14 = Rewrite.applyRuleAtId(f13, 16, Rules.privateMemory)
    val f15 = Rewrite.applyRuleAtId(f14, 13, Rules.privateMemory)

    // TODO: Cross validation
    println(Compile(f15))
  }
}

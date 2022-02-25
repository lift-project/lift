package opencl.generator.barrier_insertion_resources

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.ir._
import opencl.ir.pattern._

object VGGLayer0LambdaExpectedAfterBarrierInsertion {
  val id = UserFun("id", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
  val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
  val mult = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)
  val lambda: Lambda2 = fun(ArrayType(ArrayType(ArrayType(Float, Cst(226)), Cst(226)), Cst(3)), ArrayType(ArrayType(ArrayType(ArrayType(Float, Cst(3)), Cst(3)), Cst(3)), Cst(64)),(p_0, p_1) => FunCall(Map(fun((p_2) => FunCall(Map(fun((p_3) => FunCall(Join(), p_3))), p_2))), FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(Join(), p_6))), FunCall(Join(), FunCall(Map(fun((p_7) => FunCall(TransposeW(), p_7))), p_5))))), p_4))), FunCall(Map(fun((p_8) => FunCall(TransposeW(), FunCall(Map(fun((p_9) => FunCall(TransposeW(), p_9))), p_8)))), FunCall(MapWrg(0)(fun((p_10) => FunCall(MapSeq(fun((p_11) => FunCall(MapSeq(fun((p_12) => FunCall(fun((p_13) => FunCall(fun((p_14) => FunCall(Join(), FunCall(Split(Cst(1)), FunCall(MapSeq(fun((p_15) => FunCall(MapLcl(0)(fun((p_16) => FunCall(MapSeq(fun((p_17) => FunCall(MapSeq(fun((p_18) => FunCall(toGlobal(fun((p_19) => FunCall(id, p_19))), p_18))), FunCall(ReduceSeq(fun((p_20, p_21) => FunCall(add, p_20, p_21))), FunCall(toPrivate(fun((p_22) => FunCall(id, p_22))), Value("0.0f", Float)), p_17)))), p_16))), p_15))), FunCall(Barrier(local = false, global = true), FunCall(Join(), FunCall(Map(fun((p_23) => FunCall(Map(fun((p_24) => FunCall(Map(fun((p_25) => FunCall(Transpose(), p_25))), p_24))), p_23))), FunCall(Map(fun((p_26) => FunCall(Map(fun((p_27) => FunCall(Transpose(), p_27))), p_26))), FunCall(Map(fun((p_28) => FunCall(Transpose(), p_28))), FunCall(Transpose(), FunCall(Map(fun((p_29) => FunCall(Transpose(), p_29))), FunCall(Map(fun((p_30) => FunCall(Map(fun((p_31) => FunCall(Transpose(), p_31))), p_30))), FunCall(Map(fun((p_32) => FunCall(Map(fun((p_33) => FunCall(Map(fun((p_34) => FunCall(Transpose(), p_34))), p_33))), p_32))), FunCall(MapSeq(fun((p_35) => FunCall(Join(), FunCall(Map(fun((p_36) => FunCall(Map(fun((p_37) => FunCall(Map(fun((p_38) => FunCall(Join(), p_38))), FunCall(Join(), FunCall(Map(fun((p_39) => FunCall(TransposeW(), p_39))), p_37))))), p_36))), FunCall(Map(fun((p_40) => FunCall(TransposeW(), FunCall(Map(fun((p_41) => FunCall(TransposeW(), p_41))), p_40)))), FunCall(MapLcl(0)(fun((p_42) => FunCall(Split(Cst(4)), FunCall(MapSeq(fun((p_43) => FunCall(fun((p_44) => FunCall(fun((p_45) => FunCall(MapSeq(fun((p_46) => FunCall(MapSeq(fun((p_47) => FunCall(MapSeq(fun((p_48) => FunCall(MapSeq(fun((p_49) => FunCall(toGlobal(fun((p_50) => FunCall(id, p_50))), p_49))), FunCall(ReduceSeq(fun((p_51, p_52) => FunCall(add, p_51, FunCall(mult, FunCall(Get(0), p_52), FunCall(Get(1), p_52))))), FunCall(toPrivate(fun((p_53) => FunCall(id, p_53))), Value("0.0f", Float)), FunCall(Zip(2), FunCall(Join(), FunCall(Join(), p_48)), FunCall(Join(), FunCall(Join(), p_46))))))), p_47))), p_45))), p_44)), FunCall(MapSeq(fun((p_54) => FunCall(MapSeq(fun((p_55) => FunCall(Split(Cst(3)), FunCall(MapSeq(fun((p_56) => FunCall(MapSeq(fun((p_57) => FunCall(toGlobal(fun((p_58) => FunCall(id, p_58))), p_57))), p_56))), FunCall(Join(), p_55))))), p_54))), p_43))), p_42))), FunCall(Join(), FunCall(Map(fun((p_59) => FunCall(Transpose(), p_59))), FunCall(Split(Cst(16)), FunCall(Map(fun((p_60) => FunCall(Split(Cst(8)), p_60))), FunCall(Get(0), p_35))))))))), FunCall(Split(Cst(1)), FunCall(Get(1), p_35)))))))), FunCall(fun((p_61) => FunCall(fun((p_62) => FunCall(Zip(2), FunCall(Map(fun((p_63) => FunCall(Map(fun((p_64) => FunCall(Transpose(), p_64))), p_63))), FunCall(Map(fun((p_65) => FunCall(Transpose(), p_65))), FunCall(Map(fun((p_66) => FunCall(Map(fun((p_67) => FunCall(Get(0), p_67))), p_66))), p_62))), FunCall(Map(fun((p_68) => FunCall(Transpose(), p_68))), FunCall(Map(fun((p_69) => FunCall(Map(fun((p_70) => FunCall(Get(1), p_70))), p_69))), p_62)))), FunCall(Split(Cst(1)), FunCall(Zip(2), FunCall(Transpose(), FunCall(Map(fun((p_71) => FunCall(Transpose(), p_71))), FunCall(Get(0), p_61))), FunCall(Transpose(), FunCall(Get(1), p_61)))))), FunCall(Tuple(2), p_14, p_13)))))))))))))))), FunCall(Barrier(local = false, global = true), FunCall(MapSeq(fun((p_72) => FunCall(MapLcl(0)(fun((p_73) => FunCall(MapSeq(fun((p_74) => FunCall(Split(Cst(3)), FunCall(MapSeq(fun((p_75) => FunCall(toGlobal(fun((p_76) => FunCall(id, p_76))), p_75))), FunCall(Join(), p_74))))), p_73))), p_72))), p_12)))), FunCall(MapLcl(0)(fun((p_77) => FunCall(MapSeq(fun((p_78) => FunCall(Split(Cst(3)), FunCall(MapSeq(fun((p_79) => FunCall(toGlobal(fun((p_80) => FunCall(id, p_80))), p_79))), FunCall(Join(), p_78))))), p_77))), p_10)))), p_11))), FunCall(Map(fun((p_81) => FunCall(Transpose(), p_81))), FunCall(Split(Cst(224)), FunCall(Map(fun((p_82) => FunCall(Split(Cst(32)), p_82))), FunCall(Map(fun((p_83) => FunCall(Transpose(), p_83))), FunCall(Transpose(), FunCall(Map(fun((p_84) => FunCall(Map(fun((p_85) => FunCall(Transpose(), p_85))), FunCall(Slide(Cst(3), Cst(1)), FunCall(Map(fun((p_86) => FunCall(Slide(Cst(3), Cst(1)), p_86))), p_84))))), p_0))))))))), FunCall(Split(Cst(1)), p_1)))))))
}
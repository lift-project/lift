package opencl.generator.barrier_insertion_resources

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.ir._
import opencl.ir.pattern._

object VGGLayer10LambdaExpectedAfterBarrierInsertion {
  val id: UserFun = UserFun("id", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
  val add: UserFun = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
  val mult: UserFun = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)
  val lambda: Lambda2 = fun(ArrayType(ArrayType(ArrayType(Float, Cst(16)), Cst(16)), Cst(512)), ArrayType(ArrayType(ArrayType(ArrayType(Float, Cst(3)), Cst(3)), Cst(512)), Cst(512)),(p_0, p_1) => FunCall(Map(fun((p_2) => FunCall(Map(fun((p_3) => FunCall(Join(), p_3))), p_2))), FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(Join(), p_6))), FunCall(Join(), FunCall(Map(fun((p_7) => FunCall(TransposeW(), p_7))), p_5))))), p_4))), FunCall(Map(fun((p_8) => FunCall(TransposeW(), FunCall(Map(fun((p_9) => FunCall(TransposeW(), p_9))), p_8)))), FunCall(MapWrg(1)(fun((p_10) => FunCall(MapSeq(fun((p_11) => FunCall(MapWrg(0)(fun((p_12) => FunCall(fun((p_13) => FunCall(fun((p_14) => FunCall(Join(), FunCall(MapLcl(1)(fun((p_15) => FunCall(MapSeq(fun((p_16) => FunCall(Split(Cst(2)), FunCall(MapLcl(0)(fun((p_17) => FunCall(MapSeq(fun((p_18) => FunCall(toGlobal(fun((p_19) => FunCall(id, p_19))), p_18))), FunCall(ReduceSeq(fun((p_20, p_21) => FunCall(add, p_20, p_21))), FunCall(toPrivate(fun((p_22) => FunCall(id, p_22))), Value("0.0f", Float)), p_17)))), FunCall(Join(), p_16))))), p_15))), FunCall(Map(fun((p_23) => FunCall(Map(fun((p_24) => FunCall(Map(fun((p_25) => FunCall(Transpose(), p_25))), p_24))), p_23))), FunCall(Map(fun((p_26) => FunCall(Map(fun((p_27) => FunCall(Transpose(), p_27))), p_26))), FunCall(Map(fun((p_28) => FunCall(Transpose(), p_28))), FunCall(Transpose(), FunCall(Map(fun((p_29) => FunCall(Transpose(), p_29))), FunCall(Map(fun((p_30) => FunCall(Map(fun((p_31) => FunCall(Transpose(), p_31))), p_30))), FunCall(Map(fun((p_32) => FunCall(Map(fun((p_33) => FunCall(Map(fun((p_34) => FunCall(Transpose(), p_34))), p_33))), p_32))), FunCall(MapSeq(fun((p_35) => FunCall(Barrier(local = false, global = true), FunCall(Join(), FunCall(Map(fun((p_36) => FunCall(Map(fun((p_37) => FunCall(Map(fun((p_38) => FunCall(Join(), p_38))), FunCall(Join(), FunCall(Map(fun((p_39) => FunCall(TransposeW(), p_39))), p_37))))), p_36))), FunCall(Map(fun((p_40) => FunCall(TransposeW(), FunCall(Map(fun((p_41) => FunCall(TransposeW(), p_41))), p_40)))), FunCall(MapLcl(1)(fun((p_42) => FunCall(Split(Cst(1)), FunCall(MapSeq(fun((p_43) => FunCall(fun((p_44) => FunCall(fun((p_45) => FunCall(MapLcl(0)(fun((p_46) => FunCall(Split(Cst(2)), FunCall(MapSeq(fun((p_47) => FunCall(MapSeq(fun((p_48) => FunCall(toGlobal(fun((p_49) => FunCall(id, p_49))), p_48))), FunCall(ReduceSeq(fun((p_50, p_51) => FunCall(add, p_50, FunCall(mult, FunCall(Get(0), p_51), FunCall(Get(1), p_51))))), FunCall(toPrivate(fun((p_52) => FunCall(id, p_52))), Value("0.0f", Float)), FunCall(Zip(2), FunCall(Join(), FunCall(Join(), p_47)), FunCall(Join(), FunCall(Join(), p_46))))))), FunCall(Join(), p_45))))), p_44)), p_43)), FunCall(Barrier(local = false, global = true), FunCall(MapSeq(fun((p_53) => FunCall(MapSeq(fun((p_54) => FunCall(Split(Cst(3)), FunCall(MapLcl(0)(fun((p_55) => FunCall(toGlobal(fun((p_56) => FunCall(id, p_56))), p_55))), FunCall(Join(), p_54))))), p_53))), p_42))))), FunCall(Join(), FunCall(Map(fun((p_57) => FunCall(Transpose(), p_57))), FunCall(Split(Cst(14)), FunCall(Map(fun((p_58) => FunCall(Split(Cst(2)), p_58))), FunCall(Get(0), p_35))))))))), FunCall(Split(Cst(128)), FunCall(Get(1), p_35))))))))), FunCall(fun((p_59) => FunCall(fun((p_60) => FunCall(Zip(2), FunCall(Map(fun((p_61) => FunCall(Map(fun((p_62) => FunCall(Transpose(), p_62))), p_61))), FunCall(Map(fun((p_63) => FunCall(Transpose(), p_63))), FunCall(Map(fun((p_64) => FunCall(Map(fun((p_65) => FunCall(Get(0), p_65))), p_64))), p_60))), FunCall(Map(fun((p_66) => FunCall(Transpose(), p_66))), FunCall(Map(fun((p_67) => FunCall(Map(fun((p_68) => FunCall(Get(1), p_68))), p_67))), p_60)))), FunCall(Split(Cst(1)), FunCall(Zip(2), FunCall(Transpose(), FunCall(Map(fun((p_69) => FunCall(Transpose(), p_69))), FunCall(Get(0), p_59))), FunCall(Transpose(), FunCall(Get(1), p_59)))))), FunCall(Tuple(2), p_14, p_13))))))))))))), p_12)), FunCall(Barrier(local = false, global = true), FunCall(MapLcl(1)(fun((p_70) => FunCall(MapSeq(fun((p_71) => FunCall(Split(Cst(3)), FunCall(MapLcl(0)(fun((p_72) => FunCall(toGlobal(fun((p_73) => FunCall(id, p_73))), p_72))), FunCall(Join(), p_71))))), p_70))), p_10))))), p_11))), FunCall(Map(fun((p_74) => FunCall(Transpose(), p_74))), FunCall(Split(Cst(14)), FunCall(Map(fun((p_75) => FunCall(Split(Cst(2)), p_75))), FunCall(Map(fun((p_76) => FunCall(Transpose(), p_76))), FunCall(Transpose(), FunCall(Map(fun((p_77) => FunCall(Map(fun((p_78) => FunCall(Transpose(), p_78))), FunCall(Slide(Cst(3), Cst(1)), FunCall(Map(fun((p_79) => FunCall(Slide(Cst(3), Cst(1)), p_79))), p_77))))), p_0))))))))), FunCall(Split(Cst(256)), p_1)))))))
}
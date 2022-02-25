package benchmarks.conv.conv_lambdas

import arithmetic._
import lift.arithmetic._
import lift.arithmetic.simplifier._
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._
import opencl.ir.ast._
import opencl.generator.NDRange
import benchmarks.conv.tuning.DirectConvTuneParamSpaceFactory

object DirectConvRewritten {
  val id = UserFun("id", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
  val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
  val mult = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)

  val f = fun(ArrayType(ArrayType(ArrayType(Float, Var("inputChannels", RangeUnknown, Some(0))), SimplifySum(List(Var("inputWidth", RangeUnknown, Some(1)), Var("padOptRight", RangeAdd(Cst(0), SimplifySum(List(Var("padFuncX", RangeUnknown, Some(3)), (Var("inputWidth", RangeUnknown, Some(1))) / (Cst(2)))), Cst(1)), Some(12)), SimplifyProd(List(Cst(2), Var("padFuncX", RangeUnknown, Some(3))))))), SimplifySum(List(Var("inputHeight", RangeUnknown, Some(2)), Var("padOptBottom", RangeAdd(Cst(0), SimplifySum(List(Var("padFuncY", RangeUnknown, Some(4)), (Var("inputHeight", RangeUnknown, Some(2))) / (Cst(2)))), Cst(1)), Some(13)), SimplifyProd(List(Cst(2), Var("padFuncY", RangeUnknown, Some(4))))))), ArrayType(ArrayType(ArrayType(ArrayType(Float, Var("inputChannels", RangeUnknown, Some(0))), Var("kernelWidth", RangeUnknown, Some(5))), Var("kernelHeight", RangeUnknown, Some(6))), Var("numKernels", RangeUnknown, Some(9))),
    (p_0, p_1) =>
      FunCall(Map(fun((p_2) =>
        FunCall(TransposeW(), p_2))), FunCall(TransposeW(), FunCall(Map(fun((p_3) =>
        FunCall(Map(fun((p_4) =>
          FunCall(Join(), p_4))), p_3))), FunCall(Join(), FunCall(Map(fun((p_5) =>
        FunCall(Map(fun((p_6) =>
          FunCall(Map(fun((p_7) =>
            FunCall(Join(), p_7))), FunCall(Join(), FunCall(Map(fun((p_8) =>
            FunCall(TransposeW(), p_8))), p_6))))), p_5))), FunCall(Map(fun((p_9) =>
        FunCall(TransposeW(), FunCall(Map(fun((p_10) =>
          FunCall(TransposeW(), p_10))), p_9)))), FunCall(Map(fun((p_11) =>
        FunCall(Map(fun((p_12) =>
          FunCall(Map(fun((p_13) =>
            FunCall(fun((p_14) =>
              FunCall(fun((p_15) =>
                FunCall(Join(), FunCall(Map(fun((p_16) =>
                  FunCall(Map(fun((p_17) =>
                    FunCall(Map(fun((p_18) =>
                      FunCall(Join(), p_18))), FunCall(Join(), FunCall(Map(fun((p_19) =>
                      FunCall(TransposeW(), p_19))), p_17))))), p_16))), FunCall(Map(fun((p_20) =>
                  FunCall(TransposeW(), FunCall(Map(fun((p_21) =>
                    FunCall(TransposeW(), p_21))), p_20)))), FunCall(Map(fun((p_22) =>
                  FunCall(Map(fun((p_23) =>
                    FunCall(Map(fun((p_24) =>
                      FunCall(fun((p_25) =>
                        FunCall(fun((p_26) =>
                          FunCall(fun((p_27) =>
                            FunCall(fun((p_28) =>
                              FunCall(Join(), FunCall(TransposeW(), FunCall(Map(fun((p_29) =>
                                FunCall(TransposeW(), p_29))), FunCall(Map(fun((p_30) =>
                                FunCall(Map(fun((p_31) =>
                                  FunCall(TransposeW(), p_31))), p_30))), FunCall(Map(fun((p_32) =>
                                FunCall(Map(fun((p_33) =>
                                  FunCall(Map(fun((p_34) =>
                                    FunCall(Map(fun((p_35) =>
                                      FunCall(Map(fun((p_36) =>
                                        FunCall(toGlobal(fun((p_37) =>
                                          FunCall(id, p_37))), p_36))), p_35))), FunCall(ReduceSeq(fun((p_38, p_39) =>
                                      FunCall(Map(fun((p_40) =>
                                        FunCall(add, FunCall(Get(0), p_40), FunCall(Get(1), p_40)))), FunCall(Zip(2), p_38, p_39)))), FunCall(Map(fun((p_41) =>
                                      FunCall(toPrivate(fun((p_42) =>
                                        FunCall(id, p_42))), p_41))), Value("0.0f", ArrayType(Float, Cst(1)))), p_34)))), p_33))), p_32))), FunCall(Map(fun((p_43) =>
                                FunCall(Map(fun((p_44) =>
                                  FunCall(Transpose(), p_44))), p_43))), FunCall(Map(fun((p_45) =>
                                FunCall(Transpose(), p_45))), FunCall(Transpose(), FunCall(Map(fun((p_46) =>
                                p_46)), FunCall(Join(), FunCall(Map(fun((p_47) =>
                                FunCall(Map(fun((p_48) =>
                                  FunCall(fun((p_49) =>
                                    FunCall(Join(), FunCall(Map(fun((p_50) =>
                                      FunCall(Map(fun((p_51) =>
                                        FunCall(Map(fun((p_52) =>
                                          FunCall(Map(fun((p_53) =>
                                            FunCall(Map(fun((p_54) =>
                                              FunCall(toLocal(fun((p_55) =>
                                                FunCall(id, p_55))), p_54))), p_53))), p_52))), p_51))), p_50))), FunCall(ReduceSeq(fun((p_56, p_57) =>
                                      FunCall(Map(fun((p_58) =>
                                        FunCall(Map(fun((p_59) =>
                                          FunCall(Map(fun((p_60) =>
                                            FunCall(Map(fun((p_61) =>
                                              FunCall(add, FunCall(Get(0), p_61), FunCall(Get(1), p_61)))), p_60))), p_59))), p_58))),
                                        FunCall(Split(Var("seqWindowsPerThreadY", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("tileHeight", RangeAdd(Cst(1), SimplifySum(List(Cst(2), (SimplifySum(List(Var("inputHeight", RangeUnknown, Some(2)), Var("padOptBottom", RangeAdd(Cst(0), SimplifySum(List(Var("padFuncY", RangeUnknown, Some(4)), (Var("inputHeight", RangeUnknown, Some(2))) / (Cst(2)))), Cst(1)), Some(13)), SimplifyProd(List(Cst(-1), Var("kernelHeight", RangeUnknown, Some(6)))), SimplifyProd(List(Cst(2), Var("padFuncY", RangeUnknown, Some(4))))))) / (Var("kernelStrideY", RangeUnknown, Some(8))))), Cst(1)), Some(17)))), Cst(1)), Some(21))), FunCall(Split(Var("seqWindowsPerThreadX", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("tileWidth", RangeAdd(Cst(1), SimplifySum(List(Cst(2), (SimplifySum(List(Var("inputWidth", RangeUnknown, Some(1)), Var("padOptRight", RangeAdd(Cst(0), SimplifySum(List(Var("padFuncX", RangeUnknown, Some(3)), (Var("inputWidth", RangeUnknown, Some(1))) / (Cst(2)))), Cst(1)), Some(12)), SimplifyProd(List(Cst(-1), Var("kernelWidth", RangeUnknown, Some(5)))), SimplifyProd(List(Cst(2), Var("padFuncX", RangeUnknown, Some(3))))))) / (Var("kernelStrideX", RangeUnknown, Some(7))))), Cst(1)), Some(16)))), Cst(1)), Some(20))),
                                          FunCall(Split(Cst(1)), FunCall(Zip(2), FunCall(Join(), FunCall(Join(), FunCall(Join(), p_56))), FunCall(Join(), FunCall(Join(), FunCall(Join(), FunCall(Join(), FunCall(Map(fun((p_62) =>
                                            FunCall(Map(fun((p_63) =>
                                              FunCall(Map(fun((p_64) =>
                                                FunCall(Join(), p_64))), FunCall(Join(), FunCall(Map(fun((p_65) =>
                                                FunCall(TransposeW(), p_65))), p_63))))), p_62))), FunCall(Map(fun((p_66) =>
                                            FunCall(TransposeW(), FunCall(Map(fun((p_67) =>
                                              FunCall(TransposeW(), p_67))), p_66)))), FunCall(Map(fun((p_68) =>
                                            FunCall(Map(fun((p_69) =>
                                              FunCall(Map(fun((p_70) =>
                                                FunCall(fun((p_71) =>
                                                  FunCall(fun((p_72) =>
                                                    FunCall(Map(fun((p_73) =>
                                                      FunCall(Map(fun((p_74) =>
                                                        FunCall(Map(fun((p_75) =>
                                                          FunCall(ReduceSeq(fun((p_76, p_77) =>
                                                            FunCall(add, p_76, FunCall(mult, FunCall(Get(0), p_77), FunCall(Get(1), p_77))))), FunCall(toPrivate(fun((p_78) =>
                                                            FunCall(id, p_78))), Value("0.0f", Float)), FunCall(Zip(2), FunCall(Join(), FunCall(Join(), p_75)), FunCall(Join(), FunCall(Join(), p_73)))))), p_74))), p_72))), p_71)), FunCall(Map(fun((p_79) =>
                                                    FunCall(Map(fun((p_80) =>
                                                      FunCall(Transpose(), FunCall(Map(fun((p_81) =>
                                                        FunCall(Transpose(), p_81))), p_80)))), p_79))), FunCall(Map(fun((p_82) =>
                                                    FunCall(Transpose(), FunCall(Map(fun((p_83) =>
                                                      FunCall(Transpose(), p_83))), p_82)))), FunCall(Map(fun((p_84) =>
                                                    FunCall(Map(fun((p_85) =>
                                                      FunCall(Map(fun((p_86) =>
                                                        FunCall(Map(fun((p_87) =>
                                                          FunCall(Map(fun((p_88) =>
                                                            FunCall(toPrivate(fun((p_89) =>
                                                              FunCall(id, p_89))), p_88))), p_87))), p_86))), p_85))), p_84))), FunCall(Map(fun((p_90) =>
                                                    FunCall(Map(fun((p_91) =>
                                                      FunCall(Transpose(), FunCall(Map(fun((p_92) =>
                                                        FunCall(Transpose(), p_92))), p_91)))), p_90))), FunCall(Map(fun((p_93) =>
                                                    FunCall(Transpose(), FunCall(Map(fun((p_94) =>
                                                      FunCall(Transpose(), p_94))), p_93)))), p_70))))))), FunCall(Map(fun((p_95) =>
                                                  FunCall(Transpose(), FunCall(Map(fun((p_96) =>
                                                    FunCall(Transpose(), p_96))), p_95)))), FunCall(Map(fun((p_97) =>
                                                  FunCall(Map(fun((p_98) =>
                                                    FunCall(Map(fun((p_99) =>
                                                      FunCall(Map(fun((p_100) =>
                                                        FunCall(toPrivate(fun((p_101) =>
                                                          FunCall(id, p_101))), p_100))), p_99))), p_98))), p_97))), FunCall(Map(fun((p_102) =>
                                                  FunCall(Map(fun((p_103) =>
                                                    FunCall(Transpose(), p_103))), p_102))), FunCall(Map(fun((p_104) =>
                                                  FunCall(Transpose(), p_104))), p_68))))))), p_69))), FunCall(Map(fun((p_105) =>
                                              FunCall(Transpose(), p_105))), FunCall(Split(Var("inputCacheSizeY", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("seqWindowsPerThreadY", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("tileHeight", RangeAdd(Cst(1), SimplifySum(List(Cst(2), (SimplifySum(List(Var("inputHeight", RangeUnknown, Some(2)), Var("padOptBottom", RangeAdd(Cst(0), SimplifySum(List(Var("padFuncY", RangeUnknown, Some(4)), (Var("inputHeight", RangeUnknown, Some(2))) / (Cst(2)))), Cst(1)), Some(13)), SimplifyProd(List(Cst(-1), Var("kernelHeight", RangeUnknown, Some(6)))), SimplifyProd(List(Cst(2), Var("padFuncY", RangeUnknown, Some(4))))))) / (Var("kernelStrideY", RangeUnknown, Some(8))))), Cst(1)), Some(17)))), Cst(1)), Some(21)))), Cst(1)), Some(26))),
                                              FunCall(Map(fun((p_106) =>
                                                FunCall(Split(Var("inputCacheSizeX", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("seqWindowsPerThreadX", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("tileWidth", RangeAdd(Cst(1), SimplifySum(List(Cst(2), (SimplifySum(List(Var("inputWidth", RangeUnknown, Some(1)), Var("padOptRight", RangeAdd(Cst(0), SimplifySum(List(Var("padFuncX", RangeUnknown, Some(3)), (Var("inputWidth", RangeUnknown, Some(1))) / (Cst(2)))), Cst(1)), Some(12)), SimplifyProd(List(Cst(-1), Var("kernelWidth", RangeUnknown, Some(5)))), SimplifyProd(List(Cst(2), Var("padFuncX", RangeUnknown, Some(3))))))) / (Var("kernelStrideX", RangeUnknown, Some(7))))), Cst(1)), Some(16)))), Cst(1)), Some(20)))), Cst(1)), Some(25))), p_106))),
                                                FunCall(Get(0), p_57))))))), FunCall(Split(Var("kernelCacheSize", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("seqKernelsPerThread", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("kernelGroupSize", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("numKernels", RangeUnknown, Some(9)))), Cst(1)), Some(19)))), Cst(1)), Some(22)))), Cst(1)), Some(28))),
                                            FunCall(Get(1), p_57)))))))))))))))),
                                      FunCall(Map(fun((p_107) =>
                                        FunCall(Map(fun((p_108) =>
                                          FunCall(Map(fun((p_109) =>
                                            FunCall(Map(fun((p_110) =>
                                              FunCall(toPrivate(fun((p_111) =>
                                                FunCall(id, p_111))), p_110))), p_109))), p_108))), p_107))), Value("0.0f", ArrayType(ArrayType(ArrayType(ArrayType(Float, Cst(1)), Var("seqWindowsPerThreadX", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("tileWidth", RangeAdd(Cst(1), SimplifySum(List(Cst(2), (SimplifySum(List(Var("inputWidth", RangeUnknown, Some(1)), Var("padOptRight", RangeAdd(Cst(0), SimplifySum(List(Var("padFuncX", RangeUnknown, Some(3)), (Var("inputWidth", RangeUnknown, Some(1))) / (Cst(2)))), Cst(1)), Some(12)), SimplifyProd(List(Cst(-1), Var("kernelWidth", RangeUnknown, Some(5)))), SimplifyProd(List(Cst(2), Var("padFuncX", RangeUnknown, Some(3))))))) / (Var("kernelStrideX", RangeUnknown, Some(7))))), Cst(1)), Some(16)))), Cst(1)), Some(20))), Var("seqWindowsPerThreadY", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("tileHeight", RangeAdd(Cst(1), SimplifySum(List(Cst(2), (SimplifySum(List(Var("inputHeight", RangeUnknown, Some(2)), Var("padOptBottom", RangeAdd(Cst(0), SimplifySum(List(Var("padFuncY", RangeUnknown, Some(4)), (Var("inputHeight", RangeUnknown, Some(2))) / (Cst(2)))), Cst(1)), Some(13)), SimplifyProd(List(Cst(-1), Var("kernelHeight", RangeUnknown, Some(6)))), SimplifyProd(List(Cst(2), Var("padFuncY", RangeUnknown, Some(4))))))) / (Var("kernelStrideY", RangeUnknown, Some(8))))), Cst(1)), Some(17)))), Cst(1)), Some(21))), Var("seqKernelsPerThread", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("kernelGroupSize", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("numKernels", RangeUnknown, Some(9)))), Cst(1)), Some(19)))), Cst(1)), Some(22))))),
                                      FunCall(Join(), FunCall(Join(), FunCall(Join(), p_48))))))), p_48))), p_47))), p_27)))))))))))), p_27)), FunCall(fun((p_112) =>
                            FunCall(Map(fun((p_113) =>
                              FunCall(Map(fun((p_114) =>
                                FunCall(Map(fun((p_115) =>
                                  FunCall(Transpose(), FunCall(Map(fun((p_116) =>
                                    FunCall(Transpose(), p_116))), p_115)))), p_114))), p_113))), FunCall(Map(fun((p_117) =>
                              FunCall(Transpose(), FunCall(Map(fun((p_118) =>
                                FunCall(Transpose(), p_118))), FunCall(Map(fun((p_119) =>
                                FunCall(Map(fun((p_120) =>
                                  FunCall(Transpose(), p_120))), p_119))), p_117))))), FunCall(Transpose(), FunCall(Map(fun((p_121) =>
                              FunCall(Transpose(), p_121))), FunCall(Map(fun((p_122) =>
                              FunCall(Map(fun((p_123) =>
                                FunCall(Transpose(), p_123))), p_122))), FunCall(Map(fun((p_124) =>
                              FunCall(Map(fun((p_125) =>
                                FunCall(Map(fun((p_126) =>
                                  FunCall(Transpose(), p_126))), p_125))), p_124))), FunCall(Split(Cst(1)), FunCall(Split(Cst(1)), FunCall(Split(SimplifyProd(List(Var("tileDepth", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("inputChannels", RangeUnknown, Some(0)))), Cst(1)), Some(18)), Cst(1)/^(Var("inputCacheDepth", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("tileDepth", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("inputChannels", RangeUnknown, Some(0)))), Cst(1)), Some(18)))), Cst(1)), Some(27)))))), FunCall(Split(SimplifyProd(List(Var("kernelHeight", RangeUnknown, Some(6)), Cst(1)/^(Var("windowTileHeight", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("kernelHeight", RangeUnknown, Some(6)))), Cst(1)), Some(24)))))),
                              FunCall(Split(SimplifyProd(List(Var("kernelWidth", RangeUnknown, Some(5)), Cst(1)/^(Var("windowTileWidth", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("kernelWidth", RangeUnknown, Some(5)))), Cst(1)), Some(23)))))),
                                FunCall(Zip(2), FunCall(Join(), FunCall(Join(), FunCall(Join(), FunCall(Join(), FunCall(Join(), FunCall(Map(fun((p_127) =>
                                  FunCall(Map(fun((p_128) =>
                                    FunCall(Map(fun((p_129) =>
                                      FunCall(Map(fun((p_130) =>
                                        FunCall(Map(fun((p_131) =>
                                          FunCall(Map(fun((p_132) =>
                                            p_132)), p_131))), p_130))), FunCall(Map(fun((p_133) =>
                                        FunCall(Map(fun((p_134) =>
                                          FunCall(Map(fun((p_135) =>
                                            FunCall(Map(fun((p_136) =>
                                              FunCall(Transpose(), FunCall(Map(fun((p_137) =>
                                                FunCall(Transpose(), p_137))), FunCall(Map(fun((p_138) =>
                                                FunCall(Map(fun((p_139) =>
                                                  FunCall(Transpose(), p_139))), p_138))), p_136))))), p_135))), p_134))), p_133))), FunCall(Map(fun((p_140) =>
                                        FunCall(Map(fun((p_141) =>
                                          FunCall(Map(fun((p_142) =>
                                            FunCall(Transpose(), FunCall(Map(fun((p_143) =>
                                              FunCall(Transpose(), p_143))), FunCall(Map(fun((p_144) =>
                                              FunCall(Map(fun((p_145) =>
                                                FunCall(Transpose(), p_145))), p_144))), p_142))))), p_141))), p_140))), FunCall(Map(fun((p_146) =>
                                        FunCall(Map(fun((p_147) =>
                                          FunCall(Transpose(), FunCall(Map(fun((p_148) =>
                                            FunCall(Transpose(), p_148))), p_147)))), p_146))), FunCall(Map(fun((p_149) =>
                                        FunCall(Transpose(), p_149))), FunCall(Split(Var("inputCacheDepth", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("tileDepth", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("inputChannels", RangeUnknown, Some(0)))), Cst(1)), Some(18)))), Cst(1)), Some(27))),
                                        FunCall(Map(fun((p_150) =>
                                          FunCall(Split(Var("windowTileHeight", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("kernelHeight", RangeUnknown, Some(6)))), Cst(1)), Some(24))), p_150))),
                                          FunCall(Map(fun((p_151) =>
                                            FunCall(Map(fun((p_152) =>
                                              FunCall(Split(Var("windowTileWidth", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("kernelWidth", RangeUnknown, Some(5)))), Cst(1)), Some(23))), p_152))), p_151))),
                                            FunCall(Map(fun((p_153) =>
                                              FunCall(Map(fun((p_154) =>
                                                FunCall(Transpose(), FunCall(Map(fun((p_155) =>
                                                  FunCall(Transpose(), p_155))), p_154)))), p_153))), FunCall(Map(fun((p_156) =>
                                              FunCall(Transpose(), FunCall(Map(fun((p_157) =>
                                                FunCall(Transpose(), p_157))), p_156)))), FunCall(Transpose(), FunCall(Map(fun((p_158) =>
                                              FunCall(Transpose(), p_158))), p_129)))))))))))))), p_128))), p_127))), FunCall(Map(fun((p_159) =>
                                  FunCall(Map(fun((p_160) =>
                                    FunCall(Map(fun((p_161) =>
                                      FunCall(Map(fun((p_162) =>
                                        FunCall(Transpose(), FunCall(Map(fun((p_163) =>
                                          FunCall(Transpose(), p_163))), FunCall(Map(fun((p_164) =>
                                          FunCall(Map(fun((p_165) =>
                                            FunCall(Transpose(), p_165))), p_164))), p_162))))), p_161))), p_160))), p_159))), FunCall(Map(fun((p_166) =>
                                  FunCall(Map(fun((p_167) =>
                                    FunCall(Map(fun((p_168) =>
                                      FunCall(Transpose(), FunCall(Map(fun((p_169) =>
                                        FunCall(Transpose(), p_169))), FunCall(Map(fun((p_170) =>
                                        FunCall(Map(fun((p_171) =>
                                          FunCall(Transpose(), p_171))), p_170))), p_168))))), p_167))), p_166))), FunCall(Map(fun((p_172) =>
                                  FunCall(Map(fun((p_173) =>
                                    FunCall(Transpose(), FunCall(Map(fun((p_174) =>
                                      FunCall(Transpose(), p_174))), p_173)))), p_172))), FunCall(Map(fun((p_175) =>
                                  FunCall(Transpose(), p_175))), FunCall(Split(Var("tileDepth", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("inputChannels", RangeUnknown, Some(0)))), Cst(1)), Some(18))),
                                  FunCall(Map(fun((p_176) =>
                                    FunCall(Split(Var("kernelHeight", RangeUnknown, Some(6))), p_176))), FunCall(Map(fun((p_177) =>
                                    FunCall(Map(fun((p_178) =>
                                      FunCall(Split(Var("kernelWidth", RangeUnknown, Some(5))), p_178))), p_177))), FunCall(Map(fun((p_179) =>
                                    FunCall(Map(fun((p_180) =>
                                      FunCall(Transpose(), FunCall(Map(fun((p_181) =>
                                        FunCall(Transpose(), p_181))), p_180)))), p_179))), FunCall(Map(fun((p_182) =>
                                    FunCall(Transpose(), FunCall(Map(fun((p_183) =>
                                      FunCall(Transpose(), p_183))), p_182)))), FunCall(Transpose(), FunCall(Map(fun((p_184) =>
                                    FunCall(Transpose(), p_184))), FunCall(Get(0), p_112)))))))))))))))))), FunCall(Join(), FunCall(Join(), FunCall(Join(), FunCall(Join(), FunCall(Join(), FunCall(Map(fun((p_185) =>
                                  FunCall(Map(fun((p_186) =>
                                    FunCall(Map(fun((p_187) =>
                                      FunCall(Map(fun((p_188) =>
                                        FunCall(Map(fun((p_189) =>
                                          FunCall(Map(fun((p_190) =>
                                            p_190)), p_189))), p_188))), FunCall(Map(fun((p_191) =>
                                        FunCall(Map(fun((p_192) =>
                                          FunCall(Map(fun((p_193) =>
                                            FunCall(Transpose(), FunCall(Map(fun((p_194) =>
                                              FunCall(Transpose(), p_194))), FunCall(Map(fun((p_195) =>
                                              FunCall(Map(fun((p_196) =>
                                                FunCall(Transpose(), p_196))), p_195))), p_193))))), p_192))), p_191))), FunCall(Map(fun((p_197) =>
                                        FunCall(Map(fun((p_198) =>
                                          FunCall(Transpose(), FunCall(Map(fun((p_199) =>
                                            FunCall(Transpose(), p_199))), p_198)))), p_197))), FunCall(Map(fun((p_200) =>
                                        FunCall(Transpose(), p_200))), FunCall(Split(Var("inputCacheDepth", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("tileDepth", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("inputChannels", RangeUnknown, Some(0)))), Cst(1)), Some(18)))), Cst(1)), Some(27))),
                                        FunCall(Map(fun((p_201) =>
                                          FunCall(Split(Var("windowTileHeight", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("kernelHeight", RangeUnknown, Some(6)))), Cst(1)), Some(24))), p_201))),
                                          FunCall(Map(fun((p_202) =>
                                            FunCall(Map(fun((p_203) =>
                                              FunCall(Split(Var("windowTileWidth", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("kernelWidth", RangeUnknown, Some(5)))), Cst(1)), Some(23))), p_203))), p_202))),
                                            FunCall(Map(fun((p_204) =>
                                              FunCall(Map(fun((p_205) =>
                                                FunCall(Transpose(), p_205))), p_204))), FunCall(Map(fun((p_206) =>
                                              FunCall(Transpose(), p_206))), FunCall(Transpose(), p_187)))))))))))), p_186))), p_185))), FunCall(Map(fun((p_207) =>
                                  FunCall(Map(fun((p_208) =>
                                    FunCall(Map(fun((p_209) =>
                                      FunCall(Transpose(), FunCall(Map(fun((p_210) =>
                                        FunCall(Transpose(), p_210))), FunCall(Map(fun((p_211) =>
                                        FunCall(Map(fun((p_212) =>
                                          FunCall(Transpose(), p_212))), p_211))), p_209))))), p_208))), p_207))), FunCall(Map(fun((p_213) =>
                                  FunCall(Map(fun((p_214) =>
                                    FunCall(Transpose(), FunCall(Map(fun((p_215) =>
                                      FunCall(Transpose(), p_215))), p_214)))), p_213))), FunCall(Map(fun((p_216) =>
                                  FunCall(Transpose(), p_216))), FunCall(Split(Var("tileDepth", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("inputChannels", RangeUnknown, Some(0)))), Cst(1)), Some(18))),
                                  FunCall(Map(fun((p_217) =>
                                    FunCall(Split(Var("kernelHeight", RangeUnknown, Some(6))), p_217))), FunCall(Map(fun((p_218) =>
                                    FunCall(Map(fun((p_219) =>
                                      FunCall(Split(Var("kernelWidth", RangeUnknown, Some(5))), p_219))), p_218))), FunCall(Map(fun((p_220) =>
                                    FunCall(Map(fun((p_221) =>
                                      FunCall(Transpose(), p_221))), p_220))), FunCall(Map(fun((p_222) =>
                                    FunCall(Transpose(), p_222))), FunCall(Transpose(), FunCall(Get(1), p_112))))))))))))))))))))))))))))), FunCall(Tuple(2), p_26, p_25)))), p_24)), p_22))), p_23))), FunCall(Map(fun((p_223) =>
                    FunCall(Transpose(), p_223))), FunCall(Split(Var("seqWindowsPerThreadY", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("tileHeight", RangeAdd(Cst(1), SimplifySum(List(Cst(2), (SimplifySum(List(Var("inputHeight", RangeUnknown, Some(2)), Var("padOptBottom", RangeAdd(Cst(0), SimplifySum(List(Var("padFuncY", RangeUnknown, Some(4)), (Var("inputHeight", RangeUnknown, Some(2))) / (Cst(2)))), Cst(1)), Some(13)), SimplifyProd(List(Cst(-1), Var("kernelHeight", RangeUnknown, Some(6)))), SimplifyProd(List(Cst(2), Var("padFuncY", RangeUnknown, Some(4))))))) / (Var("kernelStrideY", RangeUnknown, Some(8))))), Cst(1)), Some(17)))), Cst(1)), Some(21))),
                    FunCall(Map(fun((p_224) =>
                      FunCall(Split(Var("seqWindowsPerThreadX", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("tileWidth", RangeAdd(Cst(1), SimplifySum(List(Cst(2), (SimplifySum(List(Var("inputWidth", RangeUnknown, Some(1)), Var("padOptRight", RangeAdd(Cst(0), SimplifySum(List(Var("padFuncX", RangeUnknown, Some(3)), (Var("inputWidth", RangeUnknown, Some(1))) / (Cst(2)))), Cst(1)), Some(12)), SimplifyProd(List(Cst(-1), Var("kernelWidth", RangeUnknown, Some(5)))), SimplifyProd(List(Cst(2), Var("padFuncX", RangeUnknown, Some(3))))))) / (Var("kernelStrideX", RangeUnknown, Some(7))))), Cst(1)), Some(16)))), Cst(1)), Some(20))), p_224))), p_15)))))),
                  FunCall(Split(Var("seqKernelsPerThread", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("kernelGroupSize", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("numKernels", RangeUnknown, Some(9)))), Cst(1)), Some(19)))), Cst(1)), Some(22))), p_14)))))), p_13)), p_11))), p_12))),
          FunCall(Map(fun((p_225) =>
            FunCall(Transpose(), p_225))), FunCall(Split(Var("tileHeight", RangeAdd(Cst(1), SimplifySum(List(Cst(2), (SimplifySum(List(Var("inputHeight", RangeUnknown, Some(2)), Var("padOptBottom", RangeAdd(Cst(0), SimplifySum(List(Var("padFuncY", RangeUnknown, Some(4)), (Var("inputHeight", RangeUnknown, Some(2))) / (Cst(2)))), Cst(1)), Some(13)), SimplifyProd(List(Cst(-1), Var("kernelHeight", RangeUnknown, Some(6)))), SimplifyProd(List(Cst(2), Var("padFuncY", RangeUnknown, Some(4))))))) / (Var("kernelStrideY", RangeUnknown, Some(8))))), Cst(1)), Some(17))),
            FunCall(Map(fun((p_226) =>
              FunCall(Split(Var("tileWidth", RangeAdd(Cst(1), SimplifySum(List(Cst(2), (SimplifySum(List(Var("inputWidth", RangeUnknown, Some(1)), Var("padOptRight", RangeAdd(Cst(0), SimplifySum(List(Var("padFuncX", RangeUnknown, Some(3)), (Var("inputWidth", RangeUnknown, Some(1))) / (Cst(2)))), Cst(1)), Some(12)), SimplifyProd(List(Cst(-1), Var("kernelWidth", RangeUnknown, Some(5)))), SimplifyProd(List(Cst(2), Var("padFuncX", RangeUnknown, Some(3))))))) / (Var("kernelStrideX", RangeUnknown, Some(7))))), Cst(1)), Some(16))), p_226))),
              FunCall(Map(fun((p_227) =>
                FunCall(Transpose(), p_227))), FunCall(Transpose(), FunCall(Map(fun((p_228) =>
                FunCall(Map(fun((p_229) =>
                  FunCall(Transpose(), p_229))), FunCall(Slide(Var("kernelHeight", RangeUnknown, Some(6)), Var("kernelStrideY", RangeUnknown, Some(8))), FunCall(Map(fun((p_230) =>
                  FunCall(Slide(Var("kernelWidth", RangeUnknown, Some(5)), Var("kernelStrideX", RangeUnknown, Some(7))), p_230))), p_228))))), FunCall(Transpose(), FunCall(Map(fun((p_231) =>
                FunCall(Transpose(), p_231))), p_0))))))))))), FunCall(Split(Var("kernelGroupSize", RangeAdd(Cst(1), SimplifySum(List(Cst(1), Var("numKernels", RangeUnknown, Some(9)))), Cst(1)), Some(19))),
        FunCall(Map(fun((p_232) =>
          FunCall(Transpose(), FunCall(Map(fun((p_233) =>
            FunCall(Transpose(), p_233))), p_232)))), p_1))))))))))
}
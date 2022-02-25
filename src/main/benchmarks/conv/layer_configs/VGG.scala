package benchmarks.conv.layer_configs

import lift.arithmetic.{Cst, Var}

import scala.collection.immutable.ListMap

object VGG extends NetConfig {
  val name: String = "vgg"
  
  val l = LayerConfigFactory
  
  val layerConfigs: Predef.Map[Int, LayerConfig] = Map(
    0 -> LayerConfig(uid = "Conv0", values = ListMap[Var, Cst]( // Conv0 // 0
      l.inputWidth -> Cst(224), l.inputHeight -> Cst(224), l.inputChannels -> Cst(3),
      l.kernelWidth -> Cst(3), l.kernelHeight -> Cst(3), l.numKernels -> Cst(64),
      l.kernelStrideX -> Cst(1), l.kernelStrideY -> Cst(1),
      l.padFuncX -> Cst(1), l.padFuncY -> Cst(1))),
    1 -> LayerConfig(uid = "Conv2", values = ListMap[Var, Cst]( // Conv2 // 1
      l.inputWidth -> Cst(224), l.inputHeight -> Cst(224), l.inputChannels -> Cst(64),
      l.kernelWidth -> Cst(3), l.kernelHeight -> Cst(3), l.numKernels -> Cst(64),
      l.kernelStrideX -> Cst(1), l.kernelStrideY -> Cst(1),
      l.padFuncX -> Cst(1), l.padFuncY -> Cst(1))),
    2 -> LayerConfig(uid = "Conv5", values = ListMap[Var, Cst]( // Conv5 // 2
      l.inputWidth -> Cst(112), l.inputHeight -> Cst(112), l.inputChannels -> Cst(64),
      l.kernelWidth -> Cst(3), l.kernelHeight -> Cst(3), l.numKernels -> Cst(128),
      l.kernelStrideX -> Cst(1), l.kernelStrideY -> Cst(1),
      l.padFuncX -> Cst(1), l.padFuncY -> Cst(1))),
    3 -> LayerConfig(uid = "Conv7", values = ListMap[Var, Cst]( // Conv7 // 3
      l.inputWidth -> Cst(112), l.inputHeight -> Cst(112), l.inputChannels -> Cst(128),
      l.kernelWidth -> Cst(3), l.kernelHeight -> Cst(3), l.numKernels -> Cst(128),
      l.kernelStrideX -> Cst(1), l.kernelStrideY -> Cst(1),
      l.padFuncX -> Cst(1), l.padFuncY -> Cst(1))),
    4 -> LayerConfig(uid = "Conv10", values = ListMap[Var, Cst]( // Conv10 // 4
      l.inputWidth -> Cst(56), l.inputHeight -> Cst(56), l.inputChannels -> Cst(128),
      l.kernelWidth -> Cst(3), l.kernelHeight -> Cst(3), l.numKernels -> Cst(256),
      l.kernelStrideX -> Cst(1), l.kernelStrideY -> Cst(1),
      l.padFuncX -> Cst(1), l.padFuncY -> Cst(1))),
    5 -> LayerConfig(uid = "Conv12", values = ListMap[Var, Cst]( // Conv12 // 5
      l.inputWidth -> Cst(56), l.inputHeight -> Cst(56), l.inputChannels -> Cst(256),
      l.kernelWidth -> Cst(3), l.kernelHeight -> Cst(3), l.numKernels -> Cst(256),
      l.kernelStrideX -> Cst(1), l.kernelStrideY -> Cst(1),
      l.padFuncX -> Cst(1), l.padFuncY -> Cst(1))),
//    6 -> LayerConfig(uid = "Conv14", values = ListMap[Var, Cst]( // Conv14 // 6
//      l.inputWidth -> Cst(56), l.inputHeight -> Cst(56), l.inputChannels -> Cst(256),
//      l.kernelWidth -> Cst(3), l.kernelHeight -> Cst(3), l.numKernels -> Cst(256),
//      l.kernelStrideX -> Cst(1), l.kernelStrideY -> Cst(1),
//      l.padFuncX -> Cst(1), l.padFuncY -> Cst(1))),
    7 -> LayerConfig(uid = "Conv17", values = ListMap[Var, Cst]( // Conv17 // 7
      l.inputWidth -> Cst(28), l.inputHeight -> Cst(28), l.inputChannels -> Cst(256),
      l.kernelWidth -> Cst(3), l.kernelHeight -> Cst(3), l.numKernels -> Cst(512),
      l.kernelStrideX -> Cst(1), l.kernelStrideY -> Cst(1),
      l.padFuncX -> Cst(1), l.padFuncY -> Cst(1))),
    8 -> LayerConfig(uid = "Conv19", values = ListMap[Var, Cst]( // Conv19 // 8
      l.inputWidth -> Cst(28), l.inputHeight -> Cst(28), l.inputChannels -> Cst(512),
      l.kernelWidth -> Cst(3), l.kernelHeight -> Cst(3), l.numKernels -> Cst(512),
      l.kernelStrideX -> Cst(1), l.kernelStrideY -> Cst(1),
      l.padFuncX -> Cst(1), l.padFuncY -> Cst(1))),
//    9 -> LayerConfig(uid = "Conv21", values = ListMap[Var, Cst]( // Conv21 // 9
//      l.inputWidth -> Cst(28), l.inputHeight -> Cst(28), l.inputChannels -> Cst(512),
//      l.kernelWidth -> Cst(3), l.kernelHeight -> Cst(3), l.numKernels -> Cst(512),
//      l.kernelStrideX -> Cst(1), l.kernelStrideY -> Cst(1),
//      l.padFuncX -> Cst(1), l.padFuncY -> Cst(1))),
    10 -> LayerConfig(uid = "Conv24", values = ListMap[Var, Cst]( // Conv24 // 10
      l.inputWidth -> Cst(14), l.inputHeight -> Cst(14), l.inputChannels -> Cst(512),
      l.kernelWidth -> Cst(3), l.kernelHeight -> Cst(3), l.numKernels -> Cst(512),
      l.kernelStrideX -> Cst(1), l.kernelStrideY -> Cst(1),
      l.padFuncX -> Cst(1), l.padFuncY -> Cst(1)))
//    11 -> LayerConfig(uid = "Conv26", values = ListMap[Var, Cst]( // Conv26 // 11
//      l.inputWidth -> Cst(14), l.inputHeight -> Cst(14), l.inputChannels -> Cst(512),
//      l.kernelWidth -> Cst(3), l.kernelHeight -> Cst(3), l.numKernels -> Cst(512),
//      l.kernelStrideX -> Cst(1), l.kernelStrideY -> Cst(1),
//      l.padFuncX -> Cst(1), l.padFuncY -> Cst(1))),
//    12 -> LayerConfig(uid = "Conv28", values = ListMap[Var, Cst]( // Conv28 // 12
//      l.inputWidth -> Cst(14), l.inputHeight -> Cst(14), l.inputChannels -> Cst(512),
//      l.kernelWidth -> Cst(3), l.kernelHeight -> Cst(3), l.numKernels -> Cst(512),
//      l.kernelStrideX -> Cst(1), l.kernelStrideY -> Cst(1),
//      l.padFuncX -> Cst(1), l.padFuncY -> Cst(1)))
//    , 102 -> LayerConfig(uid = "Conv5_optPaddedBy2", values = ListMap[Var, Cst]( // Conv5 // 13
//      l.inputWidth -> Cst(112), l.inputHeight -> Cst(112), l.inputChannels -> Cst(64),
//      l.kernelWidth -> Cst(3), l.kernelHeight -> Cst(3), l.numKernels -> Cst(128),
//      l.kernelStrideX -> Cst(1), l.kernelStrideY -> Cst(1),
//      l.padFuncX -> Cst(2), l.padFuncY -> Cst(2)))
  )
}

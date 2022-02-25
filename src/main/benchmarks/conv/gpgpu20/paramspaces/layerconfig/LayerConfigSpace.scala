package benchmarks.conv.gpgpu20.paramspaces.layerconfig

import benchmarks.conv.gpgpu20
import benchmarks.conv.gpgpu20.paramspaces.LayerParamSpace
import benchmarks.conv.gpgpu20.settings.Settings
import benchmarks.conv.gpgpu20.{LayerConfig, utils}
import lift.arithmetic.{Cst, Var}

abstract class LayerConfigSpace(configSpace: LayerConfig,
                                constraints: gpgpu20.paramspaces.ParamConstraints)
  extends LayerParamSpace(configSpace, constraints) {

  val pythonFactoryName: String

  /* ONNX file name format */
  def getOnnxConfigPath(layerConfig: Seq[Cst], envSettings: Settings): String = {
    utils.joinPaths(
      envSettings.onnxConfigsRootDir, "onnx", "generated_files",
      configSpace.paramVector.zip(layerConfig).foldLeft("")((result, paramAndValue) =>
        result + paramAndValue._1.name + "." + s"${paramAndValue._2.evalInt}" + "."
      ) + "onnx")
  }
}
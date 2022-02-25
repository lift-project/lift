package benchmarks.conv.gpgpu20

import benchmarks.conv.gpgpu20.settings.Settings
import lift.arithmetic._

import scala.collection.immutable.ListMap

package object paramspaces {
  abstract class LayerParamSpace(val layerParams: LayerParams,
                                 constraints: ParamConstraints)
    extends ParameterSpace(layerParams.paramVector, constraints) {
    val name: String
    val dependentParameters: scala.Option[Map[String, ArithExpr]] = None
    val settings: Settings
    var spaceConfigId: Option[Int] = None

    def reifyDependentParameters(independentParams: Vector[Cst]): Option[Map[String, ArithExpr]] = {
      dependentParameters match {
        case Some(dParams) => Some(dParams.map(dParam => // TODO
          (dParam._1, ArithExpr.substitute(dParam._2, layerParams.paramVector.zip(independentParams).toMap))))
        case None => None
      }
    }
  }
}

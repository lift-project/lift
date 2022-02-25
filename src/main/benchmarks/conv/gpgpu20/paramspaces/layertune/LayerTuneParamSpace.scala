package benchmarks.conv.gpgpu20.paramspaces.layertune

import benchmarks.conv.gpgpu20.LayerTuneParams
import benchmarks.conv.gpgpu20.paramspaces.{LayerParamSpace, ParamConstraints}

abstract class LayerTuneParamSpace(tuneSpace: LayerTuneParams,
                                   constraints: ParamConstraints)
  extends LayerParamSpace(tuneSpace, constraints)
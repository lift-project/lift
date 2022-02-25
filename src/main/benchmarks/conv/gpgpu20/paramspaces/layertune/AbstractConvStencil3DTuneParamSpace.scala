package benchmarks.conv.gpgpu20.paramspaces.layertune

import benchmarks.conv.gpgpu20.ConvStencil3D.ConvStencil3DTuneParams
import benchmarks.conv.gpgpu20.paramspaces.ParamConstraints
import lift.arithmetic.Var

abstract class AbstractConvStencil3DTuneParamSpace(val tuneSpace: ConvStencil3DTuneParams,
                                                   constraints: ParamConstraints)
  extends LayerTuneParamSpace(tuneSpace = tuneSpace, constraints = constraints)
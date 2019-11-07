package backends.spatial.accel.ir.pattern

import ir.ast.{Lambda1, Lambda2}
import lift.arithmetic.{ArithExpr}

abstract class SpMemFold(override val fMap: Lambda1,
                         override val fReduce: Lambda2,
                         override val chunkSize: ArithExpr,
                         override val stride: ArithExpr,
                         override val factor: ArithExpr)
  extends AbstractSpFold(fMap, fReduce, chunkSize, stride, factor)

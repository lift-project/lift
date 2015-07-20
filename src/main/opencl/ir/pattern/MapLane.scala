package opencl.ir.pattern

import arithmetic.Var
import ir.ast.Lambda1
import opencl.ir.ast.GenerableMap

case class MapLane(override val f: Lambda1)
extends GenerableMap(f, "MapLane", Var("lane_id"))

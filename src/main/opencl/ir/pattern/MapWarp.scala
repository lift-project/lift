package opencl.ir.pattern

import arithmetic.Var
import ir.ast.Lambda1
import opencl.ir.ast.GenerableMap

case class MapWarp(override val f: Lambda1)
extends GenerableMap(f, "MapWarp", Var("warp_id"))
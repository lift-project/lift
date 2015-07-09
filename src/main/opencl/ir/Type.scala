package opencl.ir

import apart.arithmetic.Cst
import ir._

// TODO: put this in a more generic backend
object Int extends ScalarType("int", Cst(4))
object Int2 extends VectorType(Int, Cst(2))
object Int3 extends VectorType(Int, Cst(3))
object Int4 extends VectorType(Int, Cst(4))
object Int8 extends VectorType(Int, Cst(8))
object Int16 extends VectorType(Int, Cst(16))

object Float extends ScalarType("float", Cst(4))
object Float2 extends VectorType(Float, Cst(2))
object Float3 extends VectorType(Float, Cst(3))
object Float4 extends VectorType(Float, Cst(4))
object Float8 extends VectorType(Float, Cst(8))
object Float16 extends VectorType(Float, Cst(16))

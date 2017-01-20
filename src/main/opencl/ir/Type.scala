package opencl.ir

import lift.arithmetic.Cst
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

object Double extends ScalarType("double", Cst(8))
object Double2 extends VectorType(Double, Cst(2))
object Double3 extends VectorType(Double, Cst(3))
object Double4 extends VectorType(Double, Cst(4))
object Double8 extends VectorType(Double, Cst(8))
object Double16 extends VectorType(Double, Cst(16))

object IntPtr extends ScalarType("int*", Cst(4))

object Bool extends ScalarType("bool", Cst(1))

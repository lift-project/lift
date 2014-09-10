package opencl.ir

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

object jInt { def getSingleton = Int }
object jInt2 { def getSingleton = Int2 }
object jInt3 { def getSingleton = Int3 }
object jInt4 { def getSingleton = Int4 }
object jInt8 { def getSingleton = Int8 }
object jInt16 { def getSingleton = Int16 }

object jFloat { def getSingleton = Float }
object jFloat2 { def getSingleton = Float2 }
object jFloat3 { def getSingleton = Float3 }
object jFloat4 { def getSingleton = Float4 }
object jFloat8 { def getSingleton = Float8 }
object jFloat16 { def getSingleton = Float16 }
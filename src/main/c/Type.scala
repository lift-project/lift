package c

import apart.arithmetic.Cst
import ir.ScalarType

/**
  * Created by Federico on 14-Jun-16.
  */
object Type {
  object Int8 extends ScalarType("char",Cst(1))
  object Int32 extends ScalarType("int",Cst(4))
}

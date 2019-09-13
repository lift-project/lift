package backends.c.sdh.sdh_ir

import ir.ast.{AbstractMap, Lambda}
import lift.arithmetic.{PosVar, Var}

abstract class AbstractSDHMap(override val f: Lambda,
                              override val name: String,
                              val num_hw_elements: Int) extends AbstractMap(f,name, PosVar("i"))


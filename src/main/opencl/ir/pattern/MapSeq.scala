package opencl.ir.pattern

import apart.arithmetic.Var
import ir.ast.{Lambda1, isGenerable, AbstractMap}

case class MapSeq(override val f: Lambda1) extends AbstractMap(f, "MapSeq",
                                                               Var("i"))
                                                   with isGenerable

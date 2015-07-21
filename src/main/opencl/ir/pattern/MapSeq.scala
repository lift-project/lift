package opencl.ir.pattern

import apart.arithmetic.Var
import ir.ast.{Lambda, isGenerable, AbstractMap}

case class MapSeq(override val f: Lambda) extends AbstractMap(f, "MapSeq",
                                                               Var("i"))
                                                   with isGenerable

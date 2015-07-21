package opencl.ir.pattern

import apart.arithmetic.Var
import ir.ast.{isGenerable, AbstractMap, Lambda1}

case class MapWarp(override val f: Lambda1) extends AbstractMap(f, "MapWarp",
                                                                Var("warp_id"))
                                                    with isGenerable
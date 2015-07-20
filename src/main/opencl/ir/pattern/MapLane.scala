package opencl.ir.pattern

import arithmetic.Var
import ir.ast.{isGenerable, AbstractMap, Lambda1}

case class MapLane(override val f: Lambda1) extends AbstractMap(f, "MapLane",
                                                                Var("lane_id"))
                                                    with isGenerable

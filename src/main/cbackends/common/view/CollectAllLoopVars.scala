package cbackends.common.view


import core.generator.GenericAST.{CVarWithType, IntegerType}
import ir.view.{View, ViewAccess, ViewMem, ViewSplit}
import lift.arithmetic.{ArithExpr}
import cbackends.common.utils.type_lowering.TypeLowering

object CollectAllLoopVars {

  def collect_loop_vars(view: View) : Set[CVarWithType] = {
    view match {
      case ViewAccess(i, iv, _) => collect_loop_vars(iv) ++ ArithExpr.collectVars(i).map(i => CVarWithType(i.toString, IntegerType()))
      case ViewSplit(_, iv, _) => collect_loop_vars(iv)
      case ViewMem(v, t) => Set[CVarWithType]( CVarWithType(v.toString, TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(t)) ) )
    }
  }

  def apply(view :View) : Set[CVarWithType] = {

    collect_loop_vars(view)

  }

}

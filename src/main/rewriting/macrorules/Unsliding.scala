package rewriting.macrorules

import core.generator.GenericAST.VarRef
import ir.{ArrayType, ScalarType, Size}
import ir.ast.{AbstractMap, Expr, FunCall, Lambda, Slide, Unslide}
import ir.view.{View, ViewPrinter, ViewSlide}
import lift.arithmetic.{ArithExpr, StartFromRange, Var}
import rewriting.rules.Rule

object Unsliding {

  val unslideSlide = Rule("Map(Map(f)) $ <slided 2d array> => " +
    "Slide() o Map(f) o Unslide() $ <slided 2d array>", {
    case FunCall(outerMap @ AbstractMap(Lambda(outerLambdaParams,
    FunCall(innerMap: AbstractMap, innerArg), _)
    ), outerArg)
      if innerArg == outerLambdaParams.head &&
        innerMap.f.body.isConcrete(visitArgs = false) && {
        getSlideParamsIfSlided2DArray(outerArg) match {
          case None => false
          case Some((slideSize, _)) =>
            // This restricts unsliding to cases when the slided window hasn't been tiled (we can't slide it back correctly currently)
            val actualSize = innerArg.t.asInstanceOf[ArrayType with Size].size
            actualSize == slideSize
        }
      } =>

      val (size, step) = getSlideParamsIfSlided2DArray(outerArg).get
//      val actualSize = innerArg.t.asInstanceOf[ArrayType with Size].size


      Slide(size, step) o
        outerMap.copy(innerMap.f) o
        Unslide(size, step) $ outerArg
  })

  private def getSlideParamsIfSlided2DArray(expr: Expr): Option[(ArithExpr, ArithExpr)] = {
    expr.t match {
      case ArrayType(ArrayType(_: ScalarType))
        if View.getSubViews(expr.view).exists {
          case _: ViewSlide => true
          case _ => false
        } =>

        // Get all slided steps, and check if rows of the arg are offset by one of these steps

        val (slideSizes, slideSteps) = View.getSubViews(expr.view).collect {
          case ViewSlide(_, Slide(size, step), _) => (size, step)
        } .unzip

        val idx0D = Var("idx0D", StartFromRange(0))
        val idx1D = Var("idx1D", StartFromRange(0))
        // Old method, deprecated
//        val rowAheadElView = expr.view.access(idx0D).access(0)
//        val rowBheadElView = expr.view.access(idx0D + 1).access(0)
//        val distanceBetweenRows = getIdxExpr(rowBheadElView) - getIdxExpr(rowAheadElView)
//
//        slideSteps.zipWithIndex.collectFirst {
//          case (step, stepIdx) if step == distanceBetweenRows => stepIdx
//        } match {
//          case Some(stepIdx) =>
//            Some((slideSizes(stepIdx), slideSteps(stepIdx)))
//          case None => None
//        }

        slideSteps.zipWithIndex.collectFirst {
          case (step, stepIdx) if {
            // view of the element number <step> of row A
            val rowAstepthElView = expr.view.access(idx0D).access(idx1D + step)
            val rowBheadElView = expr.view.access(idx0D + 1).access(idx1D)

            getIdxExpr(rowBheadElView) == getIdxExpr(rowAstepthElView)
          } =>
            stepIdx
        } match {
          case Some(stepIdx) =>
            Some((slideSizes(stepIdx), slideSteps(stepIdx)))
          case None => None
        }

      case _ => None
    }}

  val unslideSlide2d = Rule("Map0(Map1(Map2(Map3(f)))) $ <2d-slided 4d array> => " +
    "Slide() o Map0(Slide() o Map2(f) o Unslide()) o Unslide() $ <2d-slided 4d array>", {
    case call @ FunCall(map0 @ AbstractMap(Lambda(map0Params,
    FunCall(map1 @ AbstractMap(Lambda(map1Params,
    FunCall(map2 @ AbstractMap(Lambda(map2Params,
    FunCall(map3: AbstractMap, arg3), _)
    ), arg2), _)
    ), arg1), _)
    ), arg0)
      if arg1 == map0Params.head && arg2 == map1Params.head && arg3 == map2Params.head &&
        map3.f.body.isConcrete(visitArgs = false) && {
        val call2 = call
        getSlideParamsIf2DSlided4DArray(arg0) match {
          case None => false
          case Some((slideSizeOuter, _, slideSizeInner, _)) =>
            // This restricts unsliding to cases when the slided window hasn't been tiled (we can't slide it back correctly currently)
            val actualSize1 = arg1.t.asInstanceOf[ArrayType with Size].size
            val actualSize3 = arg3.t.asInstanceOf[ArrayType with Size].size
            actualSize1 == slideSizeOuter && actualSize3 == slideSizeInner
        }
      } =>

      val (sizeOuter, stepOuter, sizeInner, stepInner) = getSlideParamsIf2DSlided4DArray(arg0).get
      //      val actualSize = innerArg.t.asInstanceOf[ArrayType with Size].size


      Slide(sizeOuter, stepOuter) o
        map0.copy(

          Slide(sizeInner, stepInner) o
            map2.copy( map3.f ) o
            Unslide(sizeInner, stepInner)

        ) o
        Unslide(sizeOuter, stepOuter) $ arg0
  })

  private def getSlideParamsIf2DSlided4DArray(expr: Expr): Option[(ArithExpr, ArithExpr, ArithExpr, ArithExpr)] = {
    expr.t match {
      case ArrayType(ArrayType(ArrayType(ArrayType(_: ScalarType))))
        if View.getSubViews(expr.view).count {
          case v: ViewSlide =>
            true
          case _ => false
        } >= 2 =>

        // Get all slided steps, and check if rows of the arg are offset by one of these steps

        val (slideSizes, slideSteps) = View.getSubViews(expr.view).collect {
          case ViewSlide(_, Slide(size, step), _) => (size, step)
        } .unzip
        val idx0D = Var("idx0D", StartFromRange(0))
        val idx1D = Var("idx1D", StartFromRange(0))
        val idx2D = Var("idx2D", StartFromRange(0))
        val idx3D = Var("idx3D", StartFromRange(0))

        val innerSlideParamsIdx = slideSteps.zipWithIndex.collectFirst {
          case (step, stepIdx) if {
            // view of the element number <step> of row A
            val rowAstepthElView = expr.view.access(idx0D).access(idx1D).access(idx2D).access(idx3D + step)
            val rowBheadElView = expr.view.access(idx0D).access(idx1D).access(idx2D + 1).access(idx3D)

            getIdxExpr(rowBheadElView) == getIdxExpr(rowAstepthElView)
          } =>
            stepIdx
        }

        innerSlideParamsIdx match {
          case None => None
          case Some(someInnerSlideParamsIdx) =>
            // Remove the params of the slide we already identified
            slideSteps.zipWithIndex.patch(someInnerSlideParamsIdx, Nil, 1).collectFirst {
              case (step, stepIdx) if {

                // view of the element number <step> of row A
                val rowAstepthElView = expr.view.access(idx0D).access(idx1D + step).access(idx2D).access(idx3D)
                val rowBheadElView = expr.view.access(idx0D + 1).access(idx1D).access(idx2D).access(idx3D)

                getIdxExpr(rowBheadElView) == getIdxExpr(rowAstepthElView)
              } => stepIdx
            } match {
              case Some(someOuterSlideParamsIdx) =>

                Some((slideSizes(someOuterSlideParamsIdx), slideSteps(someOuterSlideParamsIdx),
                  slideSizes(someInnerSlideParamsIdx), slideSteps(someInnerSlideParamsIdx)))

              case None => None
            }

        }

      case _ => None
    }
  }

  private def getIdxExpr(v: View): ArithExpr =
    ViewPrinter.emit(v) match {
      case VarRef(_, _, Some(idx), _) => idx.content
      case _ => throw new IllegalArgumentException()
    }
}

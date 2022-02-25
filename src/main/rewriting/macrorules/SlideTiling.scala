package rewriting.macrorules

import ir.ast._
import rewriting.Rewrite
import rewriting.rules.{FusionRules, Rule, Rules}
import rewriting.utils.Utils

object SlideTiling {

  /**
    * Apply tiling for 1D stencils expressed as Map(f) o Slide(n,s)
    */
  val tileStencils =
    Rule("Map(f) o Slide(n,s) => Join() o Map(Map(f) o Slide(n,s)) o Slide(u,v)", {
      case funCall@FunCall(Map(_), slideCall@FunCall(Slide(_,_), _)) =>
        val tiled = Rewrite.applyRuleAt(funCall, Rules.slideTiling, slideCall)
        val moved = Rewrite.applyRuleAt(tiled, EnablingRules.movingJoin, tiled)
        val fused = Rewrite.applyRuleAtId(moved, 1, FusionRules.mapFusion)
        fused
    })

  val tileSlide2D =
    Rule("Slide2D(n,s) => TiledSlideND(2)(n,s,?)", {
      case FunCall(Map(Lambda(Array(_), FunCall(Transpose(), _), _)),
      FunCall(Slide(n1,s1),
      FunCall(Map(Lambda(Array(_), FunCall(Slide(n2,s2), _), _)), arg)))
        if n1 == n2 && s1 == s2 =>
        val slideStep = Utils.validSlideStep(arg.t, n1-s1)
        TiledSlidedND(2)(n1,s1,slideStep) $ arg
    })

  val tile2DStencils =
    Rule("Map(Map(f)) o Slide2D(n,s) => " +
      "Map(Join()) o Join() o Map(TransposeW()) o " +
      "Map(Map(Map(Map(f))))) o " +
      "Map(Map(Slide2D(n,s))) o Slide2D(tileSizeX/Y, tileStepX/Y)", {
      case FunCall(Map(Lambda(Array(_), FunCall(Map(f), _), _)),
      FunCall(Map(Lambda(Array(_), FunCall(Transpose(), _), _)),
      FunCall(Slide(n1,s1),
      FunCall(Map(Lambda(Array(_), FunCall(Slide(n2,s2), slideArg), _)), arg))))
      =>
        val slideStep1 = Utils.validSlideStep(arg.t, n1-s1)
        val slideSize1 = slideStep1 + (n1-s1)
        val slideStep2 = Utils.validSlideStep(slideArg.t, n2-s2)
        val slideSize2 = slideStep2 + (n2-s2)
        Map(Join()) o Join() o Map(TransposeW()) o Map(Map(Map(Map(f)))) o
          Map(Map(Slide2D(n1,s1, n2, s2))) o Slide2D(slideSize1, slideStep1, slideSize2, slideStep2) $ arg
    })

  // todo @bastian implement in terms of smaller rewrite rule
  val tile2DStencilsZip =
    Rule("tile2DStencilsZip) => ", {
      case
        FunCall(Map(Lambda(_, FunCall(Map(f), _), _)),
        // zip2D
        FunCall(Map(Lambda(_, FunCall(Zip(_), _*), _)),
        FunCall(Zip(_),
        // slide2D
        FunCall(Map(Lambda(_, FunCall(Transpose(), _), _)),
        FunCall(Slide(n1,s1),
        FunCall(Map(Lambda(_, FunCall(Slide(n2,s2), slideArg), _)),
        arrayToTile
        )
        )
        ),
        //rest @ _* for multiple zip args, but we actually care
        nonTiledArray
        )
        )
        )

      =>
        val slideStep1 = Utils.validSlideStep(arrayToTile.t, n1-s1)
        val slideSize1 = slideStep1 + (n1-s1)
        val slideStep2 = Utils.validSlideStep(slideArg.t, n2-s2)
        val slideSize2 = slideStep2 + (n2-s2)

        Map(Join()) o Join() o Map(TransposeW()) o
          Map(Map(\(tile => {
            Map(Map(f)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(
              Slide2D(n1, s1, n2, s2) $ tile._0,
              tile._1)
          }))) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(
          Slide2D(slideSize1,slideStep1, slideSize2, slideStep2) $ arrayToTile,
          Slide2D(slideStep1, slideStep1, slideStep2, slideStep2) $ nonTiledArray) //use only slide step here -> no overlaps
    })

  // todo: @bastian implement in terms of smaller rewrite rule and/or write generic rule that
  // todo: matches this case and the one defined above
  val tile2DStencilsZip6 =
  Rule("tile2DStencilsZip6", {
    case
      FunCall(Map(Lambda(_, FunCall(Map(f), _), _)),
      // zip2D
      FunCall(Map(Lambda(_, FunCall(Zip(_), _*), _)),
      FunCall(Zip(_),
      // slide2D
      FunCall(Map(Lambda(_, FunCall(Transpose(), _), _)),
      FunCall(Slide(n1,s1),
      FunCall(Map(Lambda(_, FunCall(Slide(n2,s2), slideArg), _)),
      arrayToTile
      )
      )
      ),
      FunCall(Map(Lambda(_, FunCall(Transpose(), _), _)),
      FunCall(Slide(_,_),
      FunCall(Map(Lambda(_, FunCall(Slide(_,_), _), _)),
      arrayToTile2
      )
      )
      ),
      nonTiledArray1,
      nonTiledArray2,
      nonTiledArray3,
      nonTiledArray4
      )
      )
      )

    =>
      val slideStep1 = Utils.validSlideStep(arrayToTile.t, n1-s1)
      val slideSize1 = slideStep1 + (n1-s1)
      val slideStep2 = Utils.validSlideStep(slideArg.t, n2-s2)
      val slideSize2 = slideStep2 + (n2-s2)

      Map(Join()) o Join() o Map(TransposeW()) o
        Map(Map(\(tile => {
          Map(Map(f)) o Map(\(tuple => Zip(tuple._0, tuple._1, tuple._2, tuple._3, tuple._4, tuple._5))) $ Zip(
            Slide2D(n1, s1, n2, s2)  $ tile._0,
            Slide2D(n1, s1, n2, s2)  $ tile._1,
            tile._2,
            tile._3,
            tile._4,
            tile._5
          )
        }))) o Map(\(tuple => Zip(tuple._0, tuple._1, tuple._2, tuple._3, tuple._4, tuple._5))) $ Zip(
        Slide2D(slideSize1,slideStep1, slideSize2, slideStep2) $ arrayToTile,
        Slide2D(slideSize1,slideStep1, slideSize2, slideStep2) $ arrayToTile2,
        Slide2D(slideStep1, slideStep1, slideStep2, slideStep2) $ nonTiledArray1,
        Slide2D(slideStep1, slideStep1, slideStep2, slideStep2) $ nonTiledArray2,
        Slide2D(slideStep1, slideStep1, slideStep2, slideStep2) $ nonTiledArray3,
        Slide2D(slideStep1, slideStep1, slideStep2, slideStep2) $ nonTiledArray4
      ) //use only slide step here -> no overlaps
  })
}

package opencl.generator.stencil

import ir._
import ir.ast._
import opencl.executor._
import opencl.generator.stencil.acoustic.StencilUtilities._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit._


object TestStencilPPCG {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestStecilPPCG {

  def increaseAndShift() = Map(Scatter(shiftRight)) o Scatter(shiftRight) o Pad2D(1,1,Pad.Boundary.Clamp)

  // also the same as grad2d
  @Test def j2d5pt: Unit = {
    val M = 8192
    val N = 8192

    def jacobi = UserFun("jacobi", Array("top", "bottom", "left", "right", "center"),
      "return (5 * top + 12 * left + 15 * center + 5 * bottom + 12 * right) / 118;",
      Seq(Float, Float, Float, Float, Float), Float)

    def grad = UserFun("grad", Array("top", "bottom", "left", "right", "center"),
      """return center + 1.0f/sqrt(0.0001f +
        | (center-top)*(center-top) +
        | (center-bottom)*(center-bottom) +
        | (center-right)*(center-right) +
        | (center-left)*(center-left));""".stripMargin,
      Seq(Float, Float, Float, Float, Float), Float)

    val lambda = λ(
      ArrayType(ArrayType(Float, M), N),
      input => {
        increaseAndShift() o
        MapGlb(1)(MapGlb(0)(λ(nbh => {

          val (top, bottom, left, right, center) = vonNeumann5pt(nbh)

//          toGlobal(id) o toPrivate(λ(x =>
//            jacobi(x, bottom, left, right, center))) $ top

          //grad2d
          toGlobal(id) o toPrivate(λ(x =>
            grad(x, bottom, left, right, center))) $ top

        }))) o Slide2D(3, 1) $ input
      })

    val kernel = Compile(lambda)
    println(kernel)
  }

  @Test def j2d9pt: Unit = {
    val M = 8192
    val N = 8192

    def f = UserFun("jacobi", Array("NW","N","NE","W","C","E","SW","S","SE"),
      "return (7 * NW + 5 * N + 9 * NE + 12 * W + 15 * C + 12 * E + 9 * SW + 5 * S + 7 * SE) / 118;",
      Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

    val reduce = λ(
      ArrayType(ArrayType(ArrayType(Float, M), N), 1),
      input => {
        ReduceSeq(
          // 'binary reduce operator' performing the stencil computation
          λ((acc, x) => {
            // restore input type
            Split(8192) o
            // increase output and shift writes
            Map(Scatter(shiftRight)) o
              Scatter(shiftRight) o Pad2D(1,1,Pad.Boundary.Clamp)
            MapGlb(1)(MapGlb(0)(λ(nbh => {

              val (northWest, north, northEast,
                west, center, east,
                southWest, south, southEast) = moore9pt(nbh)

              toGlobal(id) o toPrivate(λ(x =>
                f(x, north, northEast,
                  west, center, east,
              southWest, south, southEast))) $ northEast

        }))) o Slide2D(3, 1) $ x}),
          // initialize output
          MapSeq(MapGlb(1)(MapGlb(0)(id))) $ input) $ input
      })

    val lambda1 = λ(
      ArrayType(ArrayType(Float, M), N),
      input => {
        increaseAndShift() o
        MapGlb(1)(MapGlb(0)(λ(nbh => {

          val (northWest, north, northEast,
            west, center, east,
            southWest, south, southEast) = moore9pt(nbh)

          toGlobal(id) o toPrivate(λ(x =>
            f(x, north, northEast,
              west, center, east,
              southWest, south, southEast))) $ northEast

        }))) o Slide2D(3, 1) $ input
      })

    val lambda2 = λ(
      ArrayType(ArrayType(Float, M), N),
      ArrayType(ArrayType(Float, 3), 3),
      (input, weights) => {
        increaseAndShift() o
        MapGlb(1)(Join() o MapGlb(0)(λ(nbh =>
          MapSeq(toGlobal(id)) o
          ReduceSeqUnroll(λ( (acc, x) =>
            multAndSumUp(acc, x._0, x._1)), 0.0f)
           $ Zip(
            Join() $ nbh,
            Join() $ weights)
        ))) o Slide2D(3,1) $ input
      }
    )

    val kernel = Compile(reduce)
    println(kernel)
  }

  @Test def gaussian: Unit = {
    val M = 8192
    val N = 8192

    val weights = Array(2.0f, 4.0f, 5.0f, 4.0f, 2.0f,
      4.0f, 9.0f, 12.0f, 9.0f, 4.0f,
      5.0f, 12.0f, 15.0f, 12.0f, 5.0f,
      4.0f, 9.0f, 12.0f, 9.0f, 4.0f,
      2.0f, 4.0f, 5.0f, 4.0f, 2.0f)

    val lambda2 = λ(
      ArrayType(ArrayType(Float, M), N),
      ArrayType(ArrayType(Float, 5), 5),
      (input, weights) => {
        // increase and shift by 2!
        Map(Scatter(shift2Right)) o Scatter(shift2Right) o Pad2D(2,2,Pad.Boundary.Clamp) o
        MapGlb(1)(Join() o MapGlb(0)(λ(nbh =>
          MapSeq(toGlobal(id)) o
          ReduceSeqUnroll(λ( (acc, x) =>
            multAndSumUp(acc, x._0, x._1)), 0.0f)
           $ Zip(
            Join() $ nbh,
            Join() $ weights)
        ))) o Slide2D(5,1) $ input
      }
    )

    def f = UserFun("jacobi", Array("NNWW", "NNW", "NN", "NNE", "NNEE",
      "NWW", "NW", "N", "NE", "NEE",
      "WW", "W", "C", "E", "EE",
      "SWW", "SW", "S", "SE", "SEE",
      "SSWW", "SSW", "SS", "SSE", "SSEE"),
      """return (2*NNWW + 4*NNW + 5*NN + 4*NNE + 2*NNEE +
        | 4*NWW + 9*NW + 12*N + 9*NE + 4*NEE +
        | 5*WW + 12*W + 15*C + 12*E + 5*EE +
        | 4*SWW + 9*SW + 12*S + 9*SE + 4*SEE +
        | 2*SSWW + 4*SSW + 5*SS + 4*SSE + 2*SSEE) / 159;""".stripMargin,
      Seq(Float, Float, Float, Float, Float,
        Float, Float, Float, Float, Float,
        Float, Float, Float, Float, Float,
        Float, Float, Float, Float, Float,
        Float, Float, Float, Float, Float), Float)

    // Scala only allows 22-tuples
    /*
    val lambda1 = λ(
      ArrayType(ArrayType(Float, M), N),
      input => {
        increaseAndShift() o
        MapGlb(1)(MapGlb(0)(λ(nbh => {

          val (nnww, nnw, nn, nne, nnee,
            nww, nw, n, nee, ne,
            ww, w, c, e, ee,
            sww, sw, s, se, see,
            ssww, ssw, ss, sse, ssee) = moore25pt(nbh)

          toGlobal(id) o toPrivate(λ(x =>
            f(x, nnw, nn, nne, nnee,
              nww, nw, n, ne, nee,
              ww, w, c, e, ee,
              sww, sw, s, se, see,
              ssww, ssw, ss, sse, ssee))) $ nnww

        }))) o Slide2D(3, 1) $ input
      })
    */

    val kernel = Compile(lambda2)
    println(kernel)
  }

  // actually its only 17pt -- S and N are not used
  @Test def j3d19pt: Unit = {
    val M = 512
    val N = 512
    val O = 512

    // [K-1] = F(ront) [K+1] = B(ack)
    def jacobi = UserFun("jacobi", Array("NWF", "NWB", "NEF", "NEB", "WF", "W",
      "WB", "F", "C", "B", "EF", "E", "EB", "SWF", "SWB", "SEF", "SEB"),
      """return (0.5*(NWF + NWB + NEF + NEB) +
        |        0.51*WF + 0.71*W + 0.91*WB + 1.21*F + 1.51*C + 1.21*B + 0.91*EF + 0.71*E + 0.51*EB +
        |        0.52*(SWF + SWB + SEF + SEB)) / 159;""".stripMargin,
      Seq(Float, Float, Float, Float, Float, Float,
        Float, Float, Float, Float, Float, Float, Float,
        Float, Float, Float, Float), Float)

    val lambda = λ(
      ArrayType(ArrayType(ArrayType(Float, M), N), O),
      input => {
        Map(Map(Scatter(shiftRight))) o
          Map(Scatter(shiftRight)) o
            Scatter(shiftRight) o
        Pad3D(1,1,1,Pad.Boundary.Clamp) o
        MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(nbh => {

          //              z     y     x
          val nwf = nbh.at(0).at(0).at(0)
          val nwb = nbh.at(2).at(0).at(0)
          val nef = nbh.at(0).at(0).at(2)
          val neb = nbh.at(2).at(0).at(2)
          val wf  = nbh.at(0).at(1).at(0)
          val w   = nbh.at(1).at(1).at(0)
          val wb  = nbh.at(2).at(1).at(0)
          val f   = nbh.at(0).at(1).at(1)
          val c   = nbh.at(1).at(1).at(1)
          val b   = nbh.at(2).at(1).at(1)
          val ef  = nbh.at(0).at(1).at(2)
          val e   = nbh.at(1).at(1).at(2)
          val eb  = nbh.at(2).at(1).at(2)
          val swf = nbh.at(0).at(2).at(0)
          val swb = nbh.at(2).at(2).at(0)
          val sef = nbh.at(0).at(2).at(2)
          val seb = nbh.at(2).at(2).at(2)

          toGlobal(id) o toPrivate(λ(x =>
            jacobi(x, nwb, nef, neb, wf, w, wb, f, c, b, ef, e, eb, swf, swb, sef, seb))) $ nwf

        })))) o Slide3D(3, 1) $ input
      })

    val kernel = Compile(lambda)
    println(kernel)
  }
}

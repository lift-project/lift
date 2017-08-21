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
              Scatter(shiftRight) o Pad2D(1,1,Pad.Boundary.Clamp) o
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

    val lambda1 = λ(
      ArrayType(ArrayType(Float, M), N),
      input => {
        Map(Scatter(shift2Right)) o Scatter(shift2Right) o Pad2D(2,2,Pad.Boundary.Clamp) o
        MapGlb(1)(MapGlb(0)(λ(nbh => {

          //               y     x
          val nnww = nbh.at(0).at(0)
          val nnw  = nbh.at(0).at(1)
          val nn   = nbh.at(0).at(2)
          val nne  = nbh.at(0).at(3)
          val nnee = nbh.at(0).at(4)

          val nww  = nbh.at(1).at(0)
          val nw   = nbh.at(1).at(1)
          val n    = nbh.at(1).at(2)
          val ne   = nbh.at(1).at(3)
          val nee  = nbh.at(1).at(4)

          val ww   = nbh.at(2).at(0)
          val w    = nbh.at(2).at(1)
          val c    = nbh.at(2).at(2)
          val e    = nbh.at(2).at(3)
          val ee   = nbh.at(2).at(4)

          val sww  = nbh.at(3).at(0)
          val sw   = nbh.at(3).at(1)
          val s    = nbh.at(3).at(2)
          val se   = nbh.at(3).at(3)
          val see  = nbh.at(3).at(4)

          val ssww = nbh.at(4).at(0)
          val ssw  = nbh.at(4).at(1)
          val ss   = nbh.at(4).at(2)
          val sse  = nbh.at(4).at(3)
          val ssee = nbh.at(4).at(4)

          toGlobal(id) o toPrivate(λ(x =>
            f(x, nnw, nn, nne, nnee,
              nww, nw, n, ne, nee,
              ww, w, c, e, ee,
              sww, sw, s, se, see,
              ssww, ssw, ss, sse, ssee))) $ nnww

        }))) o Slide2D(5, 1) $ input
      })

    val kernel = Compile(lambda1)
    println(kernel)
  }
  // actually its only 17pt -- F and B are not used
  @Test def j3d19pt: Unit = {
    val M = 512
    val N = 512
    val O = 512


    // [X-1][][] = F(ront) [X+1][][] = B(ack)
    // [][X-1][] = N(orth) [][X+1][] = S(outh)
    // [][][X-1] = W(est)  [][][X+1] = E(ast)
    def jacobi = UserFun("jacobi", Array("FNW", "FNE", "FSW", "FSE",
      "NW", "N", "NE", "W", "C", "E", "SW", "S", "SE",
      "BNW", "BNE", "BSW", "BSE"),
      """return (0.5 * (FNW + FNE + FSW + FSE) +
        |        0.51 * NW + 0.71 * N + 0.91 * NE +
        |        1.21 * W + 1.51 * C + 1.21 * E +
        |        0.91 * SW + 0.71 * S + 0.51 * SE +
        |        0.52 * (BNW + BNE + BSW + BSE)) / 159;""".stripMargin,
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
          val fnw = nbh.at(0).at(0).at(0)
          val fne = nbh.at(0).at(0).at(2)
          val fsw = nbh.at(0).at(2).at(0)
          val fse = nbh.at(0).at(2).at(2)
          val nw  = nbh.at(1).at(0).at(0)
          val n   = nbh.at(1).at(0).at(1)
          val ne  = nbh.at(1).at(0).at(2)
          val w   = nbh.at(1).at(1).at(0)
          val c   = nbh.at(1).at(1).at(1)
          val e   = nbh.at(1).at(1).at(2)
          val sw  = nbh.at(1).at(2).at(0)
          val s   = nbh.at(1).at(2).at(1)
          val se  = nbh.at(1).at(2).at(2)
          val bnw = nbh.at(2).at(0).at(0)
          val bne = nbh.at(2).at(0).at(2)
          val bsw = nbh.at(2).at(2).at(0)
          val bse = nbh.at(2).at(2).at(2)

          toGlobal(id) o toPrivate(λ(x =>
            jacobi(x, fne, fsw, fse, nw, n, ne, w, c, e, sw, s, se, bnw, bne, bsw, bse))) $ fnw

        })))) o Slide3D(3, 1) $ input
      })

    val kernel = Compile(lambda)
    println(kernel)
  }

  @Test def j3d7pt: Unit = {
    val M = 512
    val N = 512
    val O = 512

    // [X-1][][] = F(ront) [X+1][][] = B(ack)
    // [][X-1][] = N(orth) [][X+1][] = S(outh)
    // [][][X-1] = W(est)  [][][X+1] = E(ast)
    def jacobi = UserFun("jacobi", Array("C", "N", "S", "E", "W", "F", "B"),
      """return 0.161f * E + 0.162f * W +
      0.163f * S + 0.164f * N +
      0.165f * B + 0.166f * F -
      1.67f * C;""".stripMargin,
      Seq(Float, Float, Float, Float, Float, Float, Float), Float)

    val lambda = λ(
      ArrayType(ArrayType(ArrayType(Float, M), N), O),
      input => {
        Map(Map(Scatter(shiftRight))) o
          Map(Scatter(shiftRight)) o
            Scatter(shiftRight) o
        Pad3D(1,1,1,Pad.Boundary.Clamp) o
        MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(nbh => {

          val (n, s, w, e, f, b, c) = vonNeumann7pt(nbh)

          toGlobal(id) o toPrivate(λ(x =>
            jacobi(x, n, s, e, w, f, b))) $ c

        })))) o Slide3D(3, 1) $ input
      })

    val kernel = Compile(lambda)
    println(kernel)
  }

  @Test def j3d27pt: Unit = {
    val M = 512
    val N = 512
    val O = 512

    // [X-1][][] = F(ront) [X+1][][] = B(ack)
    // [][X-1][] = N(orth) [][X+1][] = S(outh)
    // [][][X-1] = W(est)  [][][X+1] = E(ast)
    def jacobi = UserFun("jacobi", Array("FNW", "FN", "FNE", "FW", "F", "FE", "FSW", "FS", "FSE",
              "NW", "N", "NE", "W", "C", "E", "SW", "S", "SE",
              "BNW", "BN", "BNE", "BW", "B", "BE", "BSW", "BS", "BSE"),
      """return (0.5 * FNW + 0.7 * FN + 0.9 * FNE +
        |        1.2 * FW + 1.5 * F + 1.2 * FE +
        |        0.9 * FSW + 0.7 * FS + 0.5 * FSE +
        |        0.51 * NW + 0.71 * N + 0.91 * NE +
        |        1.21 * W + 1.51 * C + 1.21 * E +
        |        0.91 * SW + 0.71 * S + 0.51 * SE +
        |        0.52 * BNW + 0.72 * BN + 0.92 * BNE +
        |        1.22 * BW + 1.52 * B + 1.22 * BE +
        |        0.92 * BSW + 0.72 * BS + 0.52 * BSE) / 159;""".stripMargin,
      Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float,
        Float, Float, Float, Float, Float, Float, Float, Float, Float,
        Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

    val lambda = λ(
      ArrayType(ArrayType(ArrayType(Float, M), N), O),
      input => {
        Map(Map(Scatter(shiftRight))) o
          Map(Scatter(shiftRight)) o
            Scatter(shiftRight) o
        Pad3D(1,1,1,Pad.Boundary.Clamp) o
        MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(nbh => {

          //              z     y     x
          val fnw = nbh.at(0).at(0).at(0)
          val fn  = nbh.at(0).at(0).at(1)
          val fne = nbh.at(0).at(0).at(2)
          val fw  = nbh.at(0).at(1).at(0)
          val f   = nbh.at(0).at(1).at(1)
          val fe  = nbh.at(0).at(1).at(2)
          val fsw = nbh.at(0).at(2).at(0)
          val fs  = nbh.at(0).at(2).at(1)
          val fse = nbh.at(0).at(2).at(2)

          val nw  = nbh.at(1).at(0).at(0)
          val n   = nbh.at(1).at(0).at(1)
          val ne  = nbh.at(1).at(0).at(2)
          val w   = nbh.at(1).at(1).at(0)
          val c   = nbh.at(1).at(1).at(1)
          val e   = nbh.at(1).at(1).at(2)
          val sw  = nbh.at(1).at(2).at(0)
          val s   = nbh.at(1).at(2).at(1)
          val se  = nbh.at(1).at(2).at(2)

          val bnw = nbh.at(2).at(0).at(0)
          val bn  = nbh.at(2).at(0).at(1)
          val bne = nbh.at(2).at(0).at(2)
          val bw  = nbh.at(2).at(1).at(0)
          val b   = nbh.at(2).at(1).at(1)
          val be  = nbh.at(2).at(1).at(2)
          val bsw = nbh.at(2).at(2).at(0)
          val bs  = nbh.at(2).at(2).at(1)
          val bse = nbh.at(2).at(2).at(2)

          toGlobal(id) o toPrivate(λ(x =>
            jacobi(x, fn, fne, fw, f, fe, fsw, fs, fse,
              nw, n, ne, w, c, e, sw, s, se,
              bnw, bn, bne, bw, b, be, bsw, bs, bse))) $ fnw

        })))) o Slide3D(3, 1) $ input
      })

    val kernel = Compile(lambda)
    println(kernel)
  }

  @Test def j3d13pt: Unit = {
    val M = 512
    val N = 512
    val O = 512

    // [X-1][][] = F(ront) [X+1][][] = B(ack)
    // [][X-1][] = N(orth) [][X+1][] = S(outh)
    // [][][X-1] = W(est)  [][][X+1] = E(ast)
    def jacobi = UserFun("jacobi", Array("EE", "E", "W", "WW", "SS", "S", "N", "NN", "BB", "B", "F", "FF", "C"),
      """return 0.083f * EE + 0.083f * E + 0.083f * W + 0.083f * WW +
        |       0.083f * SS + 0.083f * S + 0.083f * N + 0.083f * NN +
        |       0.083f * BB + 0.083f * B + 0.083f * F + 0.083f * FF -
        |       0.996f * C;""".stripMargin,
      Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

    val lambda = λ(
      ArrayType(ArrayType(ArrayType(Float, M), N), O),
      input => {
        Map(Map(Scatter(shift2Right))) o
          Map(Scatter(shift2Right)) o
            Scatter(shift2Right) o
        Pad3D(2,2,2,Pad.Boundary.Clamp) o
        MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(nbh => {

          //              z     y     x
          val ee = nbh.at(2).at(2).at(4)
          val e  = nbh.at(2).at(2).at(3)
          val w  = nbh.at(2).at(2).at(1)
          val ww = nbh.at(2).at(2).at(0)
          val ss = nbh.at(2).at(4).at(2)
          val s  = nbh.at(2).at(3).at(2)
          val n  = nbh.at(2).at(1).at(2)
          val nn = nbh.at(2).at(0).at(2)
          val bb = nbh.at(4).at(2).at(2)
          val b  = nbh.at(3).at(2).at(2)
          val f  = nbh.at(1).at(2).at(2)
          val ff = nbh.at(0).at(2).at(2)
          val c  = nbh.at(2).at(2).at(2)

          toGlobal(id) o toPrivate(λ(x =>
            jacobi(x, e, w, ww, ss, s, n, nn, bb, b, f, ff, c))) $ ee

        })))) o Slide3D(5, 1) $ input
      })

    val kernel = Compile(lambda)
    println(kernel)
  }
}

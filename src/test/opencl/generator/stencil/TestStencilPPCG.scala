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

  def increaseAndShift() = Map(Scatter(Shift(1))) o Scatter(Shift(1)) o Pad2D(1,1,Pad.Boundary.Clamp)

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
            Map(Scatter(Shift(1))) o
              Scatter(Shift(1)) o Pad2D(1,1,Pad.Boundary.Clamp) o
            MapGlb(1)(MapGlb(0)(λ(nbh => {

              val (northWest, north, northEast,
                west, center, east,
                southWest, south, southEast) = moore9pt(nbh)

              toGlobal(id) o toPrivate(λ(x =>
                f(x, north, northEast,
                  west, center, east,
              southWest, south, southEast))) $ northWest

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
              southWest, south, southEast))) $ northWest

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

    val lambda3 = λ(
      ArrayType(ArrayType(Float, M), N),
      input => {
        //Map(Scatter(Shift(1))) o Scatter(Shift(1)) o Pad2D(1,1,Pad.Boundary.Clamp) o
        Untile2D() o MapWrg(1)(MapWrg(0)(λ(tile =>
          MapLcl(1)(MapLcl(0)(λ(nbh => {

          val (northWest, north, northEast,
            west, center, east,
            southWest, south, southEast) = moore9pt(nbh)

          toGlobal(id) o toPrivate(λ(x =>
            f(x, north, northEast,
              west, center, east,
              southWest, south, southEast))) $ northWest

        }))) o Slide2D(3, 1) o toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile))) o
        Slide2D(4,2) o Pad(1,1,Pad.Boundary.Clamp) $ input
      })

    val kernel = Compile(lambda3)
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
        Map(Scatter(Shift(2))) o Scatter(Shift(2)) o Pad2D(2,2,Pad.Boundary.Clamp) o
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
        Map(Scatter(Shift(2))) o Scatter(Shift(2)) o Pad2D(2,2,Pad.Boundary.Clamp) o
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
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
            Scatter(Shift(1)) o
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

  // also heat3d
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

    def heat = UserFun("heat", Array("C", "S", "N", "E", "W", "B", "F"),
      """return 0.125f * (B - 2.0f * C + F) +
        |       0.125f * (S - 2.0f * C + N) +
        |       0.125f * (E - 2.0f * C + W) + C;""".stripMargin,
      Seq(Float, Float, Float, Float, Float, Float, Float), Float)

    val lambda = λ(
      ArrayType(ArrayType(ArrayType(Float, M), N), O),
      input => {
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
            Scatter(Shift(1)) o
        Pad3D(1,1,1,Pad.Boundary.Clamp) o
        MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(nbh => {

          val (n, s, w, e, f, b, c) = vonNeumann7pt(nbh)

          toGlobal(id) o toPrivate(λ(x =>
            heat(x, n, s, e, w, f, b))) $ c

//          toGlobal(id) o toPrivate(λ(x =>
//            jacobi(x, n, s, e, w, f, b))) $ c

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
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
            Scatter(Shift(1)) o
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
        Map(Map(Scatter(Shift(2)))) o
          Map(Scatter(Shift(2))) o
            Scatter(Shift(2)) o
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

    @Test def denoise3d: Unit = {
      val M = 500
      val N = 500
      val O = 500

      // [X-1][][] = F(ront) [X+1][][] = B(ack)
      // [][X-1][] = N(orth) [][X+1][] = S(outh)
      // [][][X-1] = W(est)  [][][X+1] = E(ast)
      def denoise1 = UserFun("denoise1", Array("C", "S", "N", "E", "W", "B", "F"),
        """float epsilon = 1.0E-20f;
          | return 1.0f/sqrt (epsilon +
          |          (C - S)*(C - S) +
          |          (C - N)*(C - N) +
          |          (C - E)*(C - E) +
          |          (C - W)*(C - W) +
          |          (C - B)*(C - B) +
          |          (C - F)*(C - F));""".stripMargin,
        Seq(Float, Float, Float, Float, Float, Float, Float), Float)

      def denoise2 = UserFun("denoise2", Array("u0C", "fC", "u0S", "gS", "u0N", "gN", "u0E", "gE", "u0W", "gW",
        "u0B", "gB", "u0F", "gF", "gamma", "sigma2"),
        """  float r = u0C * fC / sigma2;
          |        r = (r * (2.38944f + r * (0.950037f + r))) /
          |            (4.65314f + r * (2.57541f + r * (1.48937f + r)));
          |  return (u0C + 5.0f * (
          |         u0S * gS + u0N * gN + u0E * gE +
          |         u0W * gW + u0B * gB + u0F * gF + gamma * fC * r)) /
          |         (1.0f + 5.0f * (gS + gN + gE + gW + gB + gF + gamma));""".stripMargin,
        Seq(Float, Float, Float, Float, Float, Float, Float, Float,
          Float, Float, Float, Float, Float, Float, Float, Float), Float)

      val lambda1 = λ(
        ArrayType(ArrayType(ArrayType(Float, M), N), O),
        input => {
          Map(Map(Scatter(Shift(1)))) o
            Map(Scatter(Shift(1))) o
              Scatter(Shift(1)) o
          Pad3D(1,1,1,Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(nbh => {

            val (n, s, w, e, f, b, c) = vonNeumann7pt(nbh)

            toGlobal(id) o toPrivate(λ(x =>
              denoise1(x, s, n, e, w, b, f))) $ c

          })))) o Slide3D(3, 1) $ input
        })

      val lambda2 = λ(
        ArrayType(ArrayType(ArrayType(Float, M), N), O), // f
        ArrayType(ArrayType(ArrayType(Float, M), N), O), // g
        Float,                                           // gamma
        Float,                                           // sigma2
        ArrayType(ArrayType(ArrayType(Float, M), N), O), // u0
        (f, g, gamma, sigma2, u0) => {
          Map(Map(Scatter(Shift(1)))) o
            Map(Scatter(Shift(1))) o
              Scatter(Shift(1)) o
          Pad3D(1,1,1,Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(tuple => {

            val fC = tuple._0.at(1).at(1).at(1)

            val gS = tuple._1.at(1).at(2).at(1)
            val gN = tuple._1.at(1).at(0).at(1)
            val gE = tuple._1.at(1).at(1).at(2)
            val gW = tuple._1.at(1).at(1).at(0)
            val gB = tuple._1.at(2).at(1).at(1)
            val gF = tuple._1.at(0).at(1).at(1)

            val u0C = tuple._2.at(1).at(1).at(1)
            val u0S = tuple._2.at(1).at(2).at(1)
            val u0N = tuple._2.at(1).at(0).at(1)
            val u0E = tuple._2.at(1).at(1).at(2)
            val u0W = tuple._2.at(1).at(1).at(0)
            val u0B = tuple._2.at(2).at(1).at(1)
            val u0F = tuple._2.at(0).at(1).at(1)

            toGlobal(id) o toPrivate(λ(x =>
              denoise2(x, fC, u0S, gS, u0N, gN, u0E, gE, u0W, gW, u0B, gB, u0F, gF, gamma, sigma2))) $ u0C

          })))) $ Zip3D(
            Slide3D(3,1) $ f,
            Slide3D(3,1) $ g,
            Slide3D(3,1) $ u0
          )
        })

      val kernel1 = Compile(lambda1)
      val kernel2 = Compile(lambda2)
      println(kernel1)
      println(kernel2)
    }

    @Ignore // needs to allocate more than we currently can handle
    @Test def minigmg: Unit = {
      val M = 480
      val N = 480
      val O = 480

      // [X-1][][] = F(ront) [X+1][][] = B(ack)
      // [][X-1][] = N(orth) [][X+1][] = S(outh)
      // [][][X-1] = W(est)  [][][X+1] = E(ast)
      def f = UserFun("f", Array("a", "alphaC", "b", "h2inv",
      "beta_iE", "beta_iC", "beta_jS", "beta_jC", "beta_kB", "beta_kC",
      "inputC", "inputE", "inputW", "inputS", "inputN", "inputB", "inputF", "lambdaC", "rhsC"),
        """float helmholtz = a * alphaC * inputC - b * h2inv *
          |          (beta_iE * (inputE - inputC) -
          |           beta_iC * (inputC - inputW) +
          |           beta_jS * (inputS - inputC) -
          |           beta_jC * (inputC - inputN) +
          |           beta_kB * (inputB - inputC) -
          |           beta_kC * (inputC - inputF));
          |  return inputC - 2.0f * lambdaC * (helmholtz - rhsC) / 3.0f;""".stripMargin,
        Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float,
          Float, Float, Float, Float, Float, Float, Float, Float), Float)

      val lambda = λ(
        Float,                                           // a
        ArrayType(ArrayType(ArrayType(Float, M), N), O), // alpha
        Float,                                           // b
        ArrayType(ArrayType(ArrayType(Float, M), N), O), // beta_i
        ArrayType(ArrayType(ArrayType(Float, M), N), O), // beta_j
        ArrayType(ArrayType(ArrayType(Float, M), N), O), // beta_k
        Float,                                           // h2inv
        ArrayType(ArrayType(ArrayType(Float, M), N), O), // input
        ArrayType(ArrayType(ArrayType(Float, M), N), O), // lambda
        ArrayType(ArrayType(ArrayType(Float, M), N), O), // rhs
        (a, alpha, b, beta_i, beta_j, beta_k, h2inv, input, lambda, rhs) => {
          Map(Map(Scatter(Shift(1)))) o
            Map(Scatter(Shift(1))) o
              Scatter(Shift(1)) o
          Pad3D(1,1,1,Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(tuple => {

            val alphaC = tuple._0.at(1).at(1).at(1)

            val beta_iE = tuple._1.at(1).at(1).at(2)
            val beta_iC = tuple._1.at(1).at(1).at(1)

            val beta_jS = tuple._2.at(1).at(2).at(1)
            val beta_jC = tuple._2.at(1).at(1).at(1)

            val beta_kB = tuple._3.at(2).at(1).at(1)
            val beta_kC = tuple._3.at(1).at(1).at(1)

            val inputC = tuple._4.at(1).at(1).at(1)
            val inputE = tuple._4.at(1).at(1).at(2)
            val inputW = tuple._4.at(1).at(1).at(0)
            val inputS = tuple._4.at(1).at(2).at(1)
            val inputN = tuple._4.at(1).at(0).at(1)
            val inputB = tuple._4.at(2).at(1).at(1)
            val inputF = tuple._4.at(0).at(1).at(1)

            val lambdaC = tuple._5.at(1).at(1).at(1)
            val rhsC = tuple._6.at(1).at(1).at(1)


            toGlobal(id) o toPrivate(λ(x =>
              f(x, alphaC, b, h2inv, beta_iE, beta_iC, beta_jS, beta_jC, beta_kB, beta_kC,
                inputC, inputE, inputW, inputS, inputN, inputB, inputF, lambdaC, rhsC))) $ a

          })))) $ Zip3D(
            Slide3D(3,1) $ alpha, // also works with Pad3D(-1, -1, -1, b) -- we only need center value
            Slide3D(3,1) $ beta_i,
            Slide3D(3,1) $ beta_j,
            Slide3D(3,1) $ beta_k,
            Slide3D(3,1) $ input,
            Slide3D(3,1) $ lambda, // same here
            Slide3D(3,1) $ rhs // and here
          )
        })

      val kernel = Compile(lambda)
      println(kernel)
    }

    @Test def poisson3d: Unit = {
      val M = 512
      val N = 512
      val O = 512

      // [X-1][][] = F(ront) [X+1][][] = B(ack)
      // [][X-1][] = N(orth) [][X+1][] = S(outh)
      // [][][X-1] = W(est)  [][][X+1] = E(ast)
      def poisson = UserFun("jacobi", Array("C", "N", "S", "E", "W", "F", "B",
      "FN", "BN", "FS", "BS", "FW", "BW", "NW", "SW", "FE", "BE", "NE", "SE"),
        """return 2.666f * C - 0.166f * (F + B + N + S + E + W) -
          |       0.0833f * (FN + BN + FS + BS + FW + BW +
          |                  NW + SW + FE + BE + NE + SE);""".stripMargin,
        Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float,
          Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

      val lambda = λ(
        ArrayType(ArrayType(ArrayType(Float, M), N), O),
        input => {
          Map(Map(Scatter(Shift(1)))) o
            Map(Scatter(Shift(1))) o
              Scatter(Shift(1)) o
          Pad3D(1,1,1,Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(nbh => {

            //              z     y     x
            val c  = nbh.at(1).at(1).at(1)
            val n  = nbh.at(1).at(0).at(1)
            val s  = nbh.at(1).at(2).at(1)
            val e  = nbh.at(1).at(1).at(2)
            val w  = nbh.at(1).at(1).at(0)
            val f  = nbh.at(0).at(1).at(1)
            val b  = nbh.at(2).at(1).at(1)
            val fn = nbh.at(0).at(0).at(1)
            val bn = nbh.at(2).at(0).at(1)
            val fs = nbh.at(0).at(2).at(1)
            val bs = nbh.at(2).at(2).at(1)
            val fw = nbh.at(0).at(1).at(0)
            val bw = nbh.at(2).at(1).at(0)
            val nw = nbh.at(1).at(0).at(0)
            val sw = nbh.at(1).at(2).at(0)
            val fe = nbh.at(0).at(1).at(2)
            val be = nbh.at(2).at(1).at(2)
            val ne = nbh.at(1).at(0).at(2)
            val se = nbh.at(1).at(2).at(2)


            toGlobal(id) o toPrivate(λ(x =>
              poisson(x, n, s, e, w, f, b, fn, bn, fs, bs, fw, bw, nw, sw, fe, be, ne, se))) $ c

          })))) o Slide3D(3, 1) $ input
        })

      val kernel = Compile(lambda)
      println(kernel)
    }

    @Ignore // needs to allocate more than we currently can handle
    @Test def cheby3d: Unit = {
      val M = 512
      val N = 512
      val O = 512

      // [X-1][][] = F(ront) [X+1][][] = B(ack)
      // [][X-1][] = N(orth) [][X+1][] = S(outh)
      // [][][X-1] = W(est)  [][][X+1] = E(ast)
      def cheby = UserFun("cheby", Array("AcC", "c1", "ApC", "c2", "DinvC",
        "RHS_C", "h2inv", "AcFNW", "AcFNE", "AcFSW", "AcFSE",
        "AcBNW", "AcBNE", "AcBSW", "AcBSE", "AcFN", "AcFW", "AcFE", "AcFS",
        "AcNW", "AcNE", "AcSW", "AcSE", "AcBN", "AcBW", "AcBE", "AcBS",
        "AcF", "AcN", "AcW", "AcE", "AcS", "AcB"),
        """return AcC + c1 * (AcC - ApC) + c2 * DinvC * (RHS_C -
        |           (AcC - h2inv * (0.03f * (
        |           AcFNW + AcFNE + AcFSW + AcFSE +
        |           AcBNW + AcBNE + AcBSW + AcBSE) + 0.1f * (
        |           AcFN + AcFW + AcFE + AcFS +
        |           AcNW + AcNE + AcSW + AcSE +
        |           AcBN + AcBW + AcBE + AcBS) + 0.46f * (
        |           AcF + AcN + AcW + AcE + AcS + AcB) - 4.26f * AcC)));""".stripMargin,
      Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float, Float,
        Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float,
        Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

    val lambda = λ(
      ArrayType(ArrayType(ArrayType(Float, M), N), O), // Ac
      ArrayType(ArrayType(ArrayType(Float, M), N), O), // Ap
      ArrayType(ArrayType(ArrayType(Float, M), N), O), // Dinv
      ArrayType(ArrayType(ArrayType(Float, M), N), O), // RHS
      Float,                                           // c1
      Float,                                           // c2
      Float,                                           // h2inv
      (Ac, Ap, Dinv, RHS, c1, c2, h2inv) => {
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
            Scatter(Shift(1)) o
        Pad3D(1,1,1,Pad.Boundary.Clamp) o
        MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(tuple => {

          val ac   = tuple._0
          val ap   = tuple._1
          val dinv = tuple._2
          val rhs  = tuple._3
          //              z     y     x
          val ApC   = ap.at(1).at(1).at(1)
          val DinvC = dinv.at(1).at(1).at(1)
          val RHS_C = rhs.at(1).at(1).at(1)

          val AcC   = ac.at(1).at(1).at(1)
          val AcFNW = ac.at(0).at(0).at(0)
          val AcFNE = ac.at(0).at(0).at(2)
          val AcFSW = ac.at(0).at(2).at(0)
          val AcFSE = ac.at(0).at(2).at(2)
          val AcBNW = ac.at(2).at(0).at(0)
          val AcBNE = ac.at(2).at(0).at(2)
          val AcBSW = ac.at(2).at(2).at(0)
          val AcBSE = ac.at(2).at(2).at(2)
          val AcFN  = ac.at(0).at(0).at(1)
          val AcFW  = ac.at(0).at(1).at(0)
          val AcFE  = ac.at(0).at(1).at(2)
          val AcFS  = ac.at(0).at(2).at(1)
          val AcNW  = ac.at(1).at(0).at(0)
          val AcNE  = ac.at(1).at(0).at(2)
          val AcSW  = ac.at(1).at(2).at(0)
          val AcSE  = ac.at(1).at(2).at(2)
          val AcBN  = ac.at(2).at(0).at(1)
          val AcBW  = ac.at(2).at(1).at(0)
          val AcBE  = ac.at(2).at(1).at(2)
          val AcBS  = ac.at(2).at(2).at(1)
          val AcF   = ac.at(0).at(1).at(1)
          val AcN   = ac.at(1).at(0).at(1)
          val AcW   = ac.at(1).at(1).at(0)
          val AcE   = ac.at(1).at(1).at(2)
          val AcS   = ac.at(1).at(2).at(1)
          val AcB   = ac.at(2).at(1).at(1)

          toGlobal(id) o toPrivate(λ(x =>
            cheby(x, c1, ApC, c2, DinvC, RHS_C, h2inv, AcFNW, AcFNE, AcFSW, AcFSE, AcBNW,
              AcBNE, AcBSW, AcBSE, AcFN, AcFW, AcFE, AcFS, AcNW, AcNE, AcSW, AcSE, AcBN, AcBW,
              AcBE, AcBS, AcF, AcN, AcW, AcE, AcS, AcB))) $ AcC

        })))) $ Zip3D(
          Slide3D(3,1) $ Ac,
          Slide3D(3,1) $ Ap,
          Slide3D(3,1) $ Dinv,
          Slide3D(3,1) $ RHS
        )
      })

    val kernel = Compile(lambda)
    println(kernel)
  }

  @Test def fdtd: Unit = {
    val M = 300
    val N = 300
    val O = 300

    // [X-1][][] = F(ront) [X+1][][] = B(ack)
    // [][X-1][] = N(orth) [][X+1][] = S(outh)
    // [][][X-1] = W(est)  [][][X+1] = E(ast)
    def fdtd1 = UserFun("fdtd1", Array("ExC", "HzC", "HzN", "HyC", "HyW"),
      """return 0.0002f * ExC + 0.0005f * (HzC - HzN) + 0.0005f * (HyC - HyW);""".stripMargin,
      Seq(Float, Float, Float, Float, Float), Float)

    def fdtd2 = UserFun("fdtd2", Array("EyC", "HxC", "HxW", "HzC", "HzF"),
      """return 0.0008f * EyC + 0.0005f * (HxC - HxW) + 0.0002f * (HzC - HzF);""".stripMargin,
      Seq(Float, Float, Float, Float, Float), Float)

    def fdtd3 = UserFun("fdtd3", Array("EzC", "HyC", "HyF", "HzC", "HzN"),
      """return 0.0004f * EzC + 0.0002f * (HyC - HyF) + 0.0005f * (HzC - HzN);""".stripMargin,
      Seq(Float, Float, Float, Float, Float), Float)

    def fdtd4 = UserFun("fdtd4", Array("HxC", "EzS", "EzC", "EyE", "EyC"),
      """return HxC + 0.0003f * (EzS - EzC) + 0.0007f * (EyE - EyC);""".stripMargin,
      Seq(Float, Float, Float, Float, Float), Float)

    def fdtd5 = UserFun("fdtd5", Array("HxC", "EzS", "EzC", "EyE", "EyC"),
      """return HyC + 0.0004f * (ExE - ExC) + 0.0008f * (EzB - EzC);""".stripMargin,
      Seq(Float, Float, Float, Float, Float), Float)

    def fdtd6 = UserFun("fdtd5", Array("HxC", "EzS", "EzC", "EyE", "EyC"),
      """return HzC + 0.0005f * (EyB - EyC) + 0.0009f * (EzS - EzC);""".stripMargin,
      Seq(Float, Float, Float, Float, Float), Float)

    val lambda1 = λ(
      ArrayType(ArrayType(ArrayType(Float, M), N), O), // Ex
      ArrayType(ArrayType(ArrayType(Float, M), N), O), // Hz
      ArrayType(ArrayType(ArrayType(Float, M), N), O), // Hy
      (Ex, Hz, Hy) => {
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
            Scatter(Shift(1)) o
        Pad3D(1,1,1,Pad.Boundary.Clamp) o
        MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(tuple => {

          //                    z     y     x
          val ExC = tuple._0.at(1).at(1).at(1)
          val HzC = tuple._1.at(1).at(1).at(1)
          val HzN = tuple._1.at(1).at(0).at(1)
          val HyC = tuple._2.at(1).at(1).at(1)
          val HyW = tuple._2.at(1).at(1).at(0)

          toGlobal(id) o toPrivate(λ(x =>
            fdtd1(x, HzC, HzN, HyC, HyW))) $ ExC

        })))) $ Zip3D(
          Slide3D(3,1) $ Ex,
          Slide3D(3,1) $ Hz,
          Slide3D(3,1) $ Hy
        )
      })

    val kernel1 = Compile(lambda1)
    println(kernel1)
  }
}

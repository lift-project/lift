package opencl.generator.stencil.tiling25D

import opencl.executor.TestWithExecutor
import ir.{ArrayType, ArrayTypeWSWC}
import ir.ast.{Get, Slide, Zip, fun, _}
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.generator.stencil.acoustic.{BoundaryUtilities, RoomConstants, StencilUtilities}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._
import rewriting.SimplifyAndFuse

import scala.collection.parallel
import scala.collection.parallel.immutable

/**
  */

object Test25DTilingJacobi extends TestWithExecutor




class Test25DTilingJacobi {

  @Test
  def jacobi7ptMSSComparison(): Unit =
  {

    val localDimX = 8
    val localDimY = 6
    val localDimZ = 4

    /*
    val localDimX = 510
    val localDimY = 510
    val localDimZ = 62
    */

    val slidesize = 3
    val slidestep = 1

    val data = StencilUtilities.createDataFloat3DInOrder(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))

    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

    val Nx = localDimX
    val Ny = localDimY
    val Nz = localDimZ

    val O = SizeVar("O")
    val N = SizeVar("N")
    val M = SizeVar("M")

    def original7ptJacobi(a: Int, b: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O), N), M),
      (mat) => {
        (MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {

          val `tile[0][0][0]` = m.at(0).at(0).at(0)

          val `tile[1][1][1]` = m.at(1).at(1).at(1)
          val `tile[0][1][1]` = m.at(0).at(1).at(1)
          val `tile[1][0][1]` = m.at(1).at(0).at(1)
          val `tile[1][1][0]` = m.at(1).at(1).at(0)
          val `tile[1][1][2]` = m.at(1).at(1).at(2)
          val `tile[1][2][1]` = m.at(1).at(2).at(1)
          val `tile[2][1][1]` = m.at(2).at(1).at(1)

          val stencil =  fun(x => add(x,`tile[0][1][1]`)) o
            fun(x => add(x,`tile[1][1][1]`)) o
            fun(x => add(x,`tile[1][0][1]`)) o
            fun(x => add(x,`tile[1][1][0]`)) o
            fun(x => add(x,`tile[1][1][2]`)) o
            fun(x => add(x,`tile[1][2][1]`)) $ `tile[2][1][1]`

          toGlobal(id) $ stencil
        })))) o Slide3D(a,b) o PadConstant3D(1,1,1,0.0f) $ mat)
      })

    def MSS7ptJacobi(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O), N), M),
      (input) =>
        MapGlb(1)(MapGlb(0)(fun(x => {
          toGlobal(MapSeqSlide(
            fun(m => {

              val `tile[0][0][0]` = m.at(0).at(0).at(0)

              val `tile[1][1][1]` = m.at(1).at(1).at(1)
              val `tile[0][1][1]` = m.at(0).at(1).at(1)
              val `tile[1][0][1]` = m.at(1).at(0).at(1)
              val `tile[1][1][0]` = m.at(1).at(1).at(0)
              val `tile[1][1][2]` = m.at(1).at(1).at(2)
              val `tile[1][2][1]` = m.at(1).at(2).at(1)
              val `tile[2][1][1]` = m.at(2).at(1).at(1)

              val stencil =  toPrivate(fun(x => add(x,`tile[0][1][1]`)) o
                fun(x => add(x,`tile[1][1][1]`)) o
                fun(x => add(x,`tile[1][0][1]`)) o
                fun(x => add(x,`tile[1][1][0]`)) o
                fun(x => add(x,`tile[1][1][2]`)) o
                fun(x => add(x,`tile[1][2][1]`))) $ `tile[2][1][1]`

              toGlobal(id) $ stencil
              //toGlobal(id) $ `tile[0][0][0]`

            }), a,b)) o Transpose() o Map(Transpose()) $ x }))) o Slide2D(a,b) o PadConstant3D(1,1,1,0.0f) $ input
    )

    val orgLambda = SimplifyAndFuse(original7ptJacobi(slidesize, slidestep))
    //val sourceOrg = Compile(orgLambda)//, NDRange(32,4,2), NDRange(n,m,1))
    val sourceOrg = Compile(orgLambda)
    println(sourceOrg)


    val lambdaMSS = SimplifyAndFuse(MSS7ptJacobi(slidesize, slidestep))
    val sourceMSS = Compile(lambdaMSS) //, NDRange(32,4,2), NDRange(n,m,1))
    //val sourceMSS = Compile(lambdaMSS,64,4,1,Nx,Ny,Nz, immutable.Map())
    println(sourceMSS)

    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](original7ptJacobi(slidesize, slidestep), data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](MSS7ptJacobi(slidesize, slidestep), data)

    assertArrayEquals(output_MSS, output_org, StencilUtilities.stencilDelta)

    /*
    StencilUtilities.print1DArrayAs3DArray(output_org,Nx,Ny,Nz)
    StencilUtilities.printOriginalAndOutput3DSame(dataUp,output_org)
    println("*******************************")
    StencilUtilities.print1DArray(output_org)
    //StencilUtilities.print1DArrayAs3DArray(output_MSS,Nx,Ny,Nz)
    */
  }

  @Test
  def jacobi17ptMSSComparison(): Unit =
  {

    val localDimX = 8
    val localDimY = 6
    val localDimZ = 4

    val slidesize = 3
    val slidestep = 1

    val data = StencilUtilities.createDataFloat3DInOrder(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))


    val O = SizeVar("O")
    val N = SizeVar("N")
    val M = SizeVar("M")

    def jacobi17 = UserFun("jacobi17", Array("FNW", "FNE", "FSW", "FSE",
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

    def original17ptJacobi (a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), O),
      input => {
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(
            nbh => {

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
                jacobi17(x, fne, fsw, fse, nw, n, ne, w, c, e, sw, s, se, bnw, bne, bsw, bse))) $ fnw

            })))) o Slide3D(3, 1) o PadConstant3D(1,1,1,0.0f) $ input
      })

    def MSS17ptJacobi(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O), N), M),
      (input) =>
        MapGlb(1)(MapGlb(0)(fun(x => {
          toGlobal(MapSeqSlide(
            fun(nbh => {

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
                jacobi17(x, fne, fsw, fse, nw, n, ne, w, c, e, sw, s, se, bnw, bne, bsw, bse))) $ fnw

            }), a,b)) o Transpose() o Map(Transpose()) $ x }))) o Slide2D(a,b) o PadConstant3D(1,1,1,0.0f) $ input)

    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](original17ptJacobi(slidesize, slidestep), data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](MSS17ptJacobi(slidesize, slidestep), data)


    StencilUtilities.printOriginalAndOutput3D(data,output_org)
    println("*******************************")
    StencilUtilities.print1DArrayAs3DArray(output_MSS,localDimX,localDimY,localDimZ)

    assertArrayEquals(output_MSS, output_org, StencilUtilities.stencilDelta)

  }


  @Test
  def jacobi13ptMSSComparison(): Unit = // leggy (order 2)
  {

    val localDimX = 8
    val localDimY = 6
    val localDimZ = 4

    val slidesize = 5
    val slidestep = 1

    val data = StencilUtilities.createDataFloat3DInOrder(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))


    val O = SizeVar("O")
    val N = SizeVar("N")
    val M = SizeVar("M")

    // [X-1][][] = F(ront) [X+1][][] = B(ack)
    // [][X-1][] = N(orth) [][X+1][] = S(outh)
    // [][][X-1] = W(est)  [][][X+1] = E(ast)
    def jacobi13 = UserFun("jacobi13", Array("EE", "E", "W", "WW", "SS", "S", "N", "NN", "BB", "B", "F", "FF", "C"),
      """return 0.083f * EE + 0.083f * E + 0.083f * W + 0.083f * WW +
        |       0.083f * SS + 0.083f * S + 0.083f * N + 0.083f * NN +
        |       0.083f * BB + 0.083f * B + 0.083f * F + 0.083f * FF -
        |       0.996f * C;""".stripMargin,
      Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

    def original13ptJacobi (a: Int ,b :Int) = fun(
        ArrayType(ArrayType(ArrayType(Float, M), N), O),
        input => {
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
                jacobi13(x, e, w, ww, ss, s, n, nn, bb, b, f, ff, c))) $ ee

            })))) o Slide3D(a, b)  o PadConstant3D(2,2,2,0.0f) $ input
        })


    def MSS13ptJacobi(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O), N), M),
      (input) =>
        MapGlb(1)(MapGlb(0)(fun(x => {
          toGlobal(MapSeqSlide(
            fun(nbh => {

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
                jacobi13(x, e, w, ww, ss, s, n, nn, bb, b, f, ff, c))) $ ee

            }), a,b)) o Transpose() o Map(Transpose()) $ x }))) o Slide2D(a,b) o PadConstant3D(2,2,2,0.0f) $ input
    )

    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](original13ptJacobi(slidesize, slidestep), data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](MSS13ptJacobi(slidesize, slidestep), data)

  }

  @Test
  def jacobi27ptMSSComparison() : Unit =
  {

    val localDimX = 8
    val localDimY = 6
    val localDimZ = 4

    val slidesize = 3
    val slidestep = 1

    val data = StencilUtilities.createDataFloat3DInOrder(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))


    val O = SizeVar("O")
    val N = SizeVar("N")
    val M = SizeVar("M")

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

    def original27ptJacobi( size : Int, step : Int) = λ(
      ArrayType(ArrayType(ArrayType(Float, M), N), O),
      input => {
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

          })))) o Slide3D(size, step) o PadConstant3D(1,1,1,0.0f)$ input
      })


    def MSS27ptJacobi(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O), N), M),
      (input) =>
        MapGlb(1)(MapGlb(0)(fun(x => {
          toGlobal(MapSeqSlide(
            fun(nbh => {

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


            }), a,b)) o Transpose() o Map(Transpose()) $ x }))) o Slide2D(a,b) o PadConstant3D(1,1,1,0.0f) $ input
    )


    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](original27ptJacobi(slidesize, slidestep), data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](MSS27ptJacobi(slidesize, slidestep), data)

  }


}

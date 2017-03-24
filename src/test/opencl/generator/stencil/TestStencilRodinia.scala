package opencl.generator.stencil

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.generator.stencil.acoustic.StencilUtilities
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.{AfterClass, BeforeClass, Ignore, Test}
import rewriting.SimplifyAndFuse
import org.junit.Assert._
import org.junit.Assume.assumeFalse

import scala.io.Source

//import scala.collection.parallel.immutable
import scala.collection.immutable
import scala.util.Random

object TestStencilRodinia {
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

object HotSpotConstants {

  val localDimX = 8
  val localDimY = 8
  val localDimZ = 4

  val t_chip = 0.0005f
  val chip_height = 0.016f
  val chip_width = 0.016f

  val FACTOR_CHIP = 0.5f
  val SPEC_HEAT_SI = 1750000
  val K_SI = 100
  val MAX_PD = 3000000
  val PRECISION = 0.001f

  val dx = chip_height / localDimX
  val dy = chip_width / localDimY
  val dz = t_chip / localDimZ

  val Cap = FACTOR_CHIP * SPEC_HEAT_SI * t_chip * dx * dy
  val Rx = dy / (2.0f * K_SI * t_chip * dx)
  val Ry = dx / (2.0f * K_SI * t_chip * dy)
  val Rz = dz / (K_SI * dx * dy)

  val max_slope = MAX_PD / (FACTOR_CHIP * t_chip * SPEC_HEAT_SI)
  val dt = PRECISION / max_slope

  val stepDivCap = dt / Cap
  val ce = stepDivCap / Rx
  val cn = stepDivCap / Ry
  val ct = stepDivCap / Rz
  val cw = ce
  val cs = cn
  val cb = ct

  val cc = 1.0f - (2.0f * ce + 2.0f * cn + 3.0f * ct)


  val compareDataHotspot3D: Array[Float] = Array(
    335.891876f,
    324.567505f,
    329.450287f,
    320.770172f,
    328.585052f,
    321.292908f,
    339.792175f,
    329.877197f,
    337.950775f,
    336.112671f,
    324.869324f,
    320.099396f,
    335.860992f,
    334.275604f,
    332.679626f,
    333.011139f,
    324.382690f,
    333.683044f,
    328.692169f,
    339.774811f,
    325.954193f,
    327.855682f,
    329.119598f,
    334.537048f,
    328.780060f,
    332.329895f,
    321.320984f,
    329.342804f,
    322.184967f,
    338.246063f,
    327.703308f,
    331.989532f,
    334.974884f,
    321.176025f,
    339.185669f,
    323.918579f,
    338.546753f,
    338.667450f,
    321.447601f,
    334.413940f,
    322.486969f,
    330.649689f,
    324.790497f,
    329.967499f,
    335.533203f,
    332.787048f,
    320.472412f,
    322.990326f,
    325.610809f,
    325.643677f,
    328.466766f,
    325.799988f,
    332.927429f,
    326.096466f,
    322.868835f,
    338.761017f,
    333.786346f,
    328.161987f,
    325.367218f,
    333.338440f,
    333.133606f,
    323.839996f,
    337.580231f,
    338.238861f,
    324.478546f,
    335.399139f,
    330.233826f,
    330.300690f,
    331.134674f,
    330.309814f,
    322.975189f,
    323.966034f,
    333.742004f,
    332.742798f,
    321.933441f,
    334.657257f,
    329.552734f,
    325.421600f,
    331.801392f,
    322.766083f,
    332.660950f,
    322.475983f,
    338.923462f,
    329.249481f,
    321.815186f,
    337.927155f,
    334.424347f,
    335.063171f,
    329.842346f,
    328.150543f,
    328.151276f,
    329.959198f,
    335.434692f,
    322.838287f,
    322.464874f,
    322.354675f,
    336.363739f,
    324.335327f,
    338.509399f,
    330.032593f,
    336.902069f,
    326.966858f,
    332.509949f,
    322.521362f,
    333.323669f,
    332.097717f,
    320.386902f,
    336.361786f,
    327.056946f,
    334.229675f,
    335.708893f,
    329.742920f,
    327.535278f,
    322.317535f,
    333.021637f,
    338.021179f,
    321.686646f,
    329.121887f,
    338.433533f,
    335.318604f,
    324.734863f,
    337.544434f,
    332.331543f,
    338.502380f,
    321.689667f,
    333.605103f,
    333.977356f,
    328.555542f,
    323.945496f,
    336.664124f,
    328.312317f,
    324.632721f,
    322.344116f,
    328.289246f,
    331.491180f,
    332.626434f,
    333.492889f,
    334.036804f,
    330.270111f,
    331.307037f,
    324.907043f,
    330.571594f,
    329.940857f,
    327.446075f,
    339.888000f,
    323.294067f,
    331.920166f,
    328.142792f,
    339.989502f,
    327.099152f,
    328.067474f,
    338.045959f,
    329.145111f,
    337.991943f,
    332.742310f,
    337.885071f,
    331.079376f,
    331.755463f,
    325.623108f,
    322.848694f,
    329.851257f,
    330.360474f,
    328.168427f,
    335.872925f,
    332.859314f,
    328.515533f,
    331.835480f,
    326.378906f,
    324.402344f,
    329.323273f,
    331.660950f,
    329.223663f,
    324.414795f,
    334.292450f,
    323.428986f,
    332.066528f,
    327.420166f,
    320.415833f,
    322.816864f,
    326.050232f,
    323.542114f,
    338.071014f,
    331.652130f,
    331.459869f,
    324.935638f,
    323.777008f,
    334.930145f,
    330.574890f,
    339.210144f,
    331.590820f,
    330.418793f,
    339.678589f,
    336.188995f,
    326.210632f,
    322.178284f,
    334.753113f,
    323.485779f,
    320.946350f,
    323.932648f,
    335.034546f,
    327.206543f,
    322.141418f,
    328.621857f,
    333.937286f,
    324.139648f,
    322.141357f,
    323.097046f,
    333.575989f,
    330.309448f,
    339.695526f,
    328.673309f,
    330.527954f,
    320.005768f,
    327.123474f,
    325.971039f,
    335.205292f,
    322.442108f,
    335.220337f,
    337.230743f,
    331.388916f,
    328.330872f,
    323.349518f,
    327.725006f,
    336.539734f,
    329.463531f,
    326.007935f,
    339.774658f,
    331.800934f,
    336.495331f,
    327.091675f,
    329.774017f,
    321.134674f,
    329.381622f,
    326.866394f,
    332.380035f,
    321.272095f,
    323.005341f,
    332.881165f,
    338.208282f,
    323.924652f,
    325.090088f,
    339.019043f,
    325.292267f,
    325.466217f,
    320.554810f,
    327.400574f,
    331.492859f,
    336.472290f,
    326.514221f,
    332.770782f,
    334.471436f,
    329.185608f,
    329.101379f,
    334.262878f,
    326.201508f,
    333.174988f
  )

  val inputTempHotspot3D: Array[Float] = Array(
    335.891876f,
    324.567505f,
    329.450287f,
    320.770172f,
    328.585052f,
    321.292908f,
    339.792175f,
    329.877197f,
    337.950775f,
    336.112671f,
    324.869324f,
    320.099396f,
    335.860992f,
    334.275604f,
    332.679626f,
    333.011139f,
    324.382690f,
    333.683044f,
    328.692169f,
    339.774811f,
    325.954193f,
    327.855682f,
    329.119598f,
    334.537048f,
    328.780060f,
    332.329895f,
    321.320984f,
    329.342804f,
    322.184967f,
    338.246063f,
    327.703308f,
    331.989532f,
    334.974884f,
    321.176025f,
    339.185669f,
    323.918579f,
    338.546753f,
    338.667450f,
    321.447601f,
    334.413940f,
    322.486969f,
    330.649689f,
    324.790497f,
    329.967499f,
    335.533203f,
    332.787048f,
    320.472412f,
    322.990326f,
    325.610809f,
    325.643677f,
    328.466766f,
    325.799988f,
    332.927429f,
    326.096466f,
    322.868835f,
    338.761017f,
    333.786346f,
    328.161987f,
    325.367218f,
    333.338440f,
    333.133606f,
    323.839996f,
    337.580231f,
    338.238861f,
    324.478546f,
    335.399139f,
    330.233826f,
    330.300690f,
    331.134674f,
    330.309814f,
    322.975189f,
    323.966034f,
    333.742004f,
    332.742798f,
    321.933441f,
    334.657257f,
    329.552734f,
    325.421600f,
    331.801392f,
    322.766083f,
    332.660950f,
    322.475983f,
    338.923462f,
    329.249481f,
    321.815186f,
    337.927155f,
    334.424347f,
    335.063171f,
    329.842346f,
    328.150543f,
    328.151276f,
    329.959198f,
    335.434692f,
    322.838287f,
    322.464874f,
    322.354675f,
    336.363739f,
    324.335327f,
    338.509399f,
    330.032593f,
    336.902069f,
    326.966858f,
    332.509949f,
    322.521362f,
    333.323669f,
    332.097717f,
    320.386902f,
    336.361786f,
    327.056946f,
    334.229675f,
    335.708893f,
    329.742920f,
    327.535278f,
    322.317535f,
    333.021637f,
    338.021179f,
    321.686646f,
    329.121887f,
    338.433533f,
    335.318604f,
    324.734863f,
    337.544434f,
    332.331543f,
    338.502380f,
    321.689667f,
    333.605103f,
    333.977356f,
    328.555542f,
    323.945496f,
    336.664124f,
    328.312317f,
    324.632721f,
    322.344116f,
    328.289246f,
    331.491180f,
    332.626434f,
    333.492889f,
    334.036804f,
    330.270111f,
    331.307037f,
    324.907043f,
    330.571594f,
    329.940857f,
    327.446075f,
    339.888000f,
    323.294067f,
    331.920166f,
    328.142792f,
    339.989502f,
    327.099152f,
    328.067474f,
    338.045959f,
    329.145111f,
    337.991943f,
    332.742310f,
    337.885071f,
    331.079376f,
    331.755463f,
    325.623108f,
    322.848694f,
    329.851257f,
    330.360474f,
    328.168427f,
    335.872925f,
    332.859314f,
    328.515533f,
    331.835480f,
    326.378906f,
    324.402344f,
    329.323273f,
    331.660950f,
    329.223663f,
    324.414795f,
    334.292450f,
    323.428986f,
    332.066528f,
    327.420166f,
    320.415833f,
    322.816864f,
    326.050232f,
    323.542114f,
    338.071014f,
    331.652130f,
    331.459869f,
    324.935638f,
    323.777008f,
    334.930145f,
    330.574890f,
    339.210144f,
    331.590820f,
    330.418793f,
    339.678589f,
    336.188995f,
    326.210632f,
    322.178284f,
    334.753113f,
    323.485779f,
    320.946350f,
    323.932648f,
    335.034546f,
    327.206543f,
    322.141418f,
    328.621857f,
    333.937286f,
    324.139648f,
    322.141357f,
    323.097046f,
    333.575989f,
    330.309448f,
    339.695526f,
    328.673309f,
    330.527954f,
    320.005768f,
    327.123474f,
    325.971039f,
    335.205292f,
    322.442108f,
    335.220337f,
    337.230743f,
    331.388916f,
    328.330872f,
    323.349518f,
    327.725006f,
    336.539734f,
    329.463531f,
    326.007935f,
    339.774658f,
    331.800934f,
    336.495331f,
    327.091675f,
    329.774017f,
    321.134674f,
    329.381622f,
    326.866394f,
    332.380035f,
    321.272095f,
    323.005341f,
    332.881165f,
    338.208282f,
    323.924652f,
    325.090088f,
    339.019043f,
    325.292267f,
    325.466217f,
    320.554810f,
    327.400574f,
    331.492859f,
    336.472290f,
    326.514221f,
    332.770782f,
    334.471436f,
    329.185608f,
    329.101379f,
    334.262878f,
    326.201508f,
    333.174988f
  )

  val inputPowerHotspot3D = Array(
    0.509597f,
    0.698461f,
    0.163586f,
    0.236186f,
    0.961582f,
    0.944245f,
    0.559009f,
    0.711667f,
    0.308460f,
    0.875817f,
    0.255831f,
    0.629395f,
    0.383964f,
    0.844122f,
    0.203740f,
    0.887715f,
    0.156436f,
    0.786553f,
    0.574846f,
    0.571970f,
    0.159446f,
    0.124089f,
    0.302347f,
    0.761025f,
    0.851024f,
    0.018463f,
    0.482234f,
    0.614696f,
    0.061564f,
    0.824071f,
    0.575040f,
    0.884210f,
    0.598722f,
    0.615092f,
    0.423641f,
    0.106467f,
    0.408234f,
    0.041533f,
    0.280055f,
    0.990330f,
    0.901837f,
    0.817424f,
    0.968817f,
    0.745069f,
    0.729789f,
    0.755152f,
    0.663404f,
    0.728503f,
    0.549080f,
    0.965988f,
    0.736396f,
    0.751760f,
    0.650075f,
    0.143616f,
    0.021110f,
    0.475702f,
    0.907946f,
    0.781805f,
    0.560216f,
    0.489503f,
    0.696023f,
    0.332291f,
    0.329720f,
    0.235196f,
    0.530504f,
    0.169909f,
    0.261784f,
    0.585235f,
    0.626526f,
    0.897851f,
    0.405223f,
    0.725497f,
    0.014745f,
    0.220095f,
    0.818025f,
    0.636291f,
    0.467694f,
    0.415620f,
    0.147487f,
    0.215909f,
    0.464486f,
    0.212731f,
    0.931652f,
    0.361045f,
    0.292109f,
    0.557488f,
    0.051987f,
    0.133992f,
    0.438478f,
    0.971451f,
    0.469422f,
    0.369438f,
    0.566504f,
    0.811900f,
    0.022281f,
    0.493730f,
    0.302713f,
    0.646575f,
    0.240438f,
    0.338364f,
    0.419900f,
    0.451396f,
    0.480802f,
    0.514344f,
    0.767314f,
    0.958260f,
    0.831298f,
    0.817321f,
    0.996778f,
    0.105516f,
    0.606669f,
    0.610700f,
    0.501280f,
    0.996805f,
    0.594788f,
    0.893325f,
    0.017379f,
    0.325616f,
    0.284284f,
    0.240562f,
    0.866423f,
    0.311650f,
    0.850967f,
    0.239916f,
    0.903462f,
    0.380851f,
    0.625518f,
    0.440688f,
    0.155624f,
    0.470216f,
    0.015551f,
    0.919655f,
    0.366184f,
    0.826023f,
    0.257675f,
    0.935957f,
    0.861388f,
    0.684457f,
    0.293303f,
    0.484589f,
    0.837963f,
    0.378151f,
    0.021162f,
    0.835169f,
    0.840030f,
    0.230868f,
    0.101026f,
    0.141273f,
    0.606231f,
    0.881134f,
    0.154882f,
    0.279558f,
    0.492406f,
    0.763702f,
    0.706115f,
    0.258225f,
    0.893840f,
    0.902932f,
    0.984781f,
    0.687180f,
    0.234731f,
    0.384243f,
    0.311720f,
    0.328616f,
    0.318280f,
    0.648739f,
    0.614231f,
    0.716114f,
    0.490951f,
    0.959470f,
    0.508046f,
    0.874823f,
    0.821929f,
    0.054804f,
    0.404447f,
    0.550752f,
    0.527031f,
    0.759935f,
    0.561496f,
    0.097291f,
    0.338836f,
    0.407045f,
    0.186291f,
    0.039242f,
    0.264483f,
    0.247250f,
    0.191220f,
    0.748734f,
    0.410195f,
    0.318795f,
    0.422440f,
    0.887058f,
    0.547762f,
    0.149467f,
    0.791408f,
    0.026230f,
    0.345581f,
    0.886976f,
    0.613067f,
    0.060573f,
    0.739289f,
    0.691448f,
    0.007366f,
    0.462891f,
    0.050986f,
    0.772045f,
    0.502013f,
    0.165353f,
    0.708180f,
    0.798686f,
    0.246565f,
    0.626273f,
    0.683627f,
    0.789565f,
    0.392114f,
    0.200008f,
    0.850039f,
    0.355576f,
    0.363968f,
    0.333257f,
    0.170464f,
    0.653822f,
    0.748295f,
    0.251683f,
    0.404866f,
    0.903189f,
    0.007899f,
    0.542390f,
    0.701767f,
    0.385060f,
    0.787704f,
    0.246284f,
    0.153490f,
    0.538543f,
    0.303745f,
    0.585533f,
    0.753224f,
    0.954146f,
    0.669825f,
    0.379986f,
    0.010361f,
    0.044413f,
    0.290359f,
    0.583052f,
    0.459289f,
    0.326889f,
    0.872605f,
    0.219364f,
    0.551606f,
    0.332292f,
    0.684558f,
    0.218062f,
    0.368606f,
    0.847901f,
    0.187862f,
    0.640609f
  )


}

class TestStencilRodinia {
  val zip2d = \((A, B) =>
    Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(A, B)
  )

  val zip2d6 = \((A, B, C, D, E, F) =>
    Map(\(tuple => Zip(tuple._0, tuple._1, tuple._2, tuple._3, tuple._4, tuple._5))) $ Zip(A, B, C, D, E, F)
  )

  /* **********************************************************
     RODINIA HOTSPOT
 ***********************************************************/
  @Test def rodiniaHotspot(): Unit = {

    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    // p = powerValue, t = heatNbh; userfun has to compute:
    // out = t[0,0] + c(p + y(t[-1,0] + t[1,0] - 2t[0,0]) +
    //                      x(t[0,-1] + t[0,1] - 2t[0,0]) +
    //                      z(a - t[0,0]));
    // a, c, x, y, z are constants
    val addAmbientTemp = UserFun("addAmbientTemp", Array("x", "y"), "{ return x + y + (0.1f * 1.068e-7f * 80.0f); }", Seq(Float, Float), Float)

    // the two outermost dimensions of A and B have to be the same
    // zip matrices elementwise

    // create 16x16 heat tile and 14x14 power tile
    val createTiles = \((heat, power) =>
      zip2d(Slide2D(16, 14) o Pad2D(1, 1, Pad.Boundary.Clamp) $ heat,
        Slide2D(14, 14) $ power)
    )

    // load into local memory and prepare data for single work-item
    // < < coeff, heat> :: [9] , power >
    val prepareData = \((coeff, tiles) =>
      zip2d(
        // first component;
        Map(Map(\(heatNbh =>
          Zip(coeff, Join() $ heatNbh)
        ))) o Slide2D(3, 1) o
          toLocal(MapLcl(1)(MapLcl(0)(id))) $ tiles._0,
        // second component
        toLocal(MapLcl(1)(MapLcl(0)(id))) $ tiles._1)
    )

    // how to compute output using required data:
    val stencil = fun(requiredData => {
      val coeffHeatTuple = requiredData._0
      val powerValue = requiredData._1

      toGlobal(MapSeq(id)) o
        MapSeq(\(x => addAmbientTemp(powerValue, x))) o
        ReduceSeqUnroll(\((acc, next) =>
          multAndSumUp(acc, next._0, next._1)), 0.0f) $ coeffHeatTuple
    })

    val rodinia = fun(
      ArrayType(ArrayType(Float, 1036), 1036),
      ArrayType(ArrayType(Float, 1036), 1036),
      //ArrayType(ArrayType(Float, 8204), 8204),
      //ArrayType(ArrayType(Float, 8204), 8204),
      ArrayType(Float, 9),
      (heat, power, coeff) => {
        MapWrg(1)(MapWrg(0)(\(tiles =>
          MapLcl(1)(MapLcl(0)(stencil)) o prepareData(coeff) $ tiles)
        )) $ createTiles(heat, power)

      }
    )
    /*
    val stencil = fun(
      ArrayType(ArrayType(Float, 1036), 1036),
      ArrayType(ArrayType(Float, 1036), 1036),
      ArrayType(Float, 9),
      (heat, power, coeff) => {
        MapWrg(1)(MapWrg(0)(
          fun(tiles => {
            val powerTile = Get(tiles, 0)
            val heatTile = Get(tiles, 1)
            MapLcl(1)(MapLcl(0)(
              fun(nbhs => {
                val powerValue = Get(nbhs, 0) // Float
                val heatNbh = Get(nbhs, 1)    // [[Float]_3]_3
                toGlobal(MapSeq(id)) o
                  MapSeq( \(x => addAmbientTemp(powerValue, x))) o
                  ReduceSeqUnroll(\((acc, next) =>
                    multAndSumUp(acc, next._0, next._1)), 0.0f) $ Zip(Join() $ heatNbh, coeff)
              })
            )) o
              Split(14) $
              Zip(Join() o toLocal(MapLcl(1)(MapLcl(0)(id))) $ powerTile,
                  Join() o Slide2D(3,1,3,1) o
                           toLocal(MapLcl(1)(MapLcl(0)(id))) $ heatTile)
          })
        )) o
          Split(74) $
          Zip(Join() o Slide2D(14,14) $ power,
              Join() o Slide2D(16,14) o
                Pad2D(1,1,Pad.Boundary.Wrap) $ heat) //actually its mirror
      }
    )
    */

    val heat = Array.tabulate(1036, 1036) { (i, j) => i * 1036.0f + j }
    val power = Array.tabulate(1036, 1036) { (i, j) => i * 1036.0f + j }
    //val heat = Array.tabulate(8204, 8204) { (i, j) => i * 8204.0f + j }
    //val power = Array.tabulate(8204, 8204) { (i, j) => i * 8204.0f + j }
    val x = 0.1f
    val y = 0.1f
    val z = 1024000
    val c = 1.068e-7f
    val coeff = Array(0, c * y, 0, c * x, c * (-2 * y - 2 * x - z + 1), c * x, 0, c * y, 0)

    val (output: Array[Float], runtime) = Execute(16, 16, 1184, 1184, (true, true))(rodinia, heat, power, coeff)
    //val (output: Array[Float], runtime) = Execute(16,16, 9376, 9376, (true, true))(rodinia, heat, power, coeff)
    println("Runtime: " + runtime)
  }

  /* **********************************************************
      HOTSPOT 3D RODINIA
  ***********************************************************/
  @Test def rodiniaHotspot3D(): Unit = {
    @Ignore //fix
    //val hotspot = UserFun("hotspot", "tuple", "{ return tuple_0; }", TupleType(Float, ArrayType(ArrayType(Float, 3),3)), Float)
    // segfaults
    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, 512), 512), 8),
      ArrayType(Float, 27),
      (input, weights) => {
        MapSeq(MapGlb(1)(MapGlb(0)(\(nbh =>

          toGlobal(MapSeq(id)) o
            ReduceSeqUnroll(\((acc, next) =>
              multAndSumUp(acc, next._0, next._1)), 0.0f) $
            Zip(Join() o Join() $ nbh,
              weights))
        ))) o Slide3D(3, 1) o Pad3D(1, 1, 1, Pad.Boundary.MirrorUnsafe) $ input
      }
    )

    // testing
    val input = Array.tabulate(512, 512, 8) { (i, j, k) => Random.nextFloat() }
    val weights = Array.tabulate(27) { (i) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(64, 4, 1, 512, 512, 1, (true, true))(stencil, input, weights)
    println("Runtime: " + runtime)
    //println(output.mkString(","))
  }

  // rodinia 3d opt1
  @Test def rodiniaHotspot3DOpt1(): Unit = {
    val zip3d = \((A, B, C) =>
      Map(Map(\(tuple => Zip(tuple._0, tuple._1, tuple._2)))) o Map(\(tuple => Zip(tuple._0, tuple._1, tuple._2))) $ Zip(A, B, C)
    )

    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, 512), 512), 8),
      (input) => {
        MapSeq(MapGlb(1)(MapGlb(0)(\(nbh =>
          toGlobal(MapSeq(\(acc => add(acc, nbh._2)))) o
            MapSeq(\(acc => add(acc, nbh._1))) o
            ReduceSeqUnroll(add, 0.0f) o Join() o Join() $ nbh._0)
        ))) $ zip3d(
          Slide3D(3, 1, 3, 1, 1, 1) o Pad3D(1, 1, 0, Pad.Boundary.Clamp) $ input,
          input, // leftshift z - upper element
          input) // rightshift z - lower element
      }
    )

    val input = Array.fill(512)(Array.fill(512)(Array.fill(8)(1.0f)))
    val weights = Array.fill(3)(Array.fill(3)(1.0f))
    val (output: Array[Float], runtime) = Execute(64, 4, 1, 512, 512, 8, (true, true))(stencil, input)
    println("Runtime: " + runtime)
    //println(output.mkString(","))
  }

  @Test def rodiniaHotspot3DLocalMemory(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, 512), 512), 8),
      (input) => {
        MapSeq(MapWrg(1)(MapWrg(0)(\(tiles =>
          MapSeq(MapLcl(1)(MapLcl(0)(\(nbh =>
            toGlobal(MapSeq(id)) o
              ReduceSeq(add, 0.0f) o Join() o Join() $ nbh)))) o
            Slide3D(3, 1) o
            toLocal(MapSeq(MapLcl(1)(MapLcl(0)(id)))) $ tiles)
        ))) o Slide3D(66, 64, 6, 4, 10, 10) o Pad3D(1, 1, 1, Pad.Boundary.Clamp) $ input
      }
    )

    // testing
    val input = Array.tabulate(512, 512, 8) { (i, j, k) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(64, 4, 1, 512, 512, 1, (true, true))(stencil, input)
    println("Runtime: " + runtime)
    //println(output.mkString(","))
  }

  @Test
  def RodiniaHotSpot3DWithData(): Unit = {

    val tempInput = HotSpotConstants.inputTempHotspot3D.sliding(HotSpotConstants.localDimX, HotSpotConstants.localDimY).toArray.sliding(HotSpotConstants.localDimX, HotSpotConstants.localDimY).toArray
    val powerInput = HotSpotConstants.inputPowerHotspot3D.sliding(HotSpotConstants.localDimX, HotSpotConstants.localDimY).toArray.sliding(HotSpotConstants.localDimX, HotSpotConstants.localDimY).toArray

    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

    val calculateHotspot = UserFun("calculateHotspot", Array("tInC", "cc", "tInN", "cn", "tInS", "cs", "tInE", "ce", "tInW", "cw", "tInT", "ct", "tInB", "cb", "stepDivCap", "pInC", "amb_temp"),
      "{ return  tInC*cc + tInN*cn + tInS*cs + tInE*ce + tInW*cw + tInT*ct + tInB*cb + stepDivCap * pInC + ct*amb_temp; }", Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

    val rodiniaHotSpot3D =
      fun(ArrayType(ArrayType(ArrayType(Float, m), n), o),
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      Float,
      Float,
      Float,
      Float,
      Float,
      Float,
      Float,
      Float,
      (temp, power, ce, cw, cn, cs, ct, cb, cc, stepDivCap) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun((m) => {

          val amb_temp = 80.0f
          val ct_amb_temp = fun(x => mult(x, ct)) $ amb_temp

          val tInC = Get(m, 1).at(1).at(1).at(1)
          val tIncCC = toPrivate(fun(x => mult(x, cc))) $ tInC

          val tInW = Get(m, 1).at(0).at(1).at(1)
          val tIncW = toPrivate(fun(x => mult(x, cw))) $ tInW

          val tInN = Get(m, 1).at(1).at(0).at(1)
          val tIncN = toPrivate(fun(x => mult(x, cn))) $ tInN

          val tInB = Get(m, 1).at(1).at(1).at(0)
          val tIncB = toPrivate(fun(x => mult(x, cb))) $ tInB

          val tInT = Get(m, 1).at(1).at(1).at(2)
          val tIncT = toPrivate(fun(x => mult(x, ct))) $ tInT

          val tInS = Get(m, 1).at(1).at(2).at(1)
          val tIncS = toPrivate(fun(x => mult(x, cs))) $ tInS

          val tInE = Get(m, 1).at(2).at(1).at(1)
          val tIncE = toPrivate(fun(x => mult(x, ce))) $ tInE

          val pInc = Get(m, 0)
          val pcSDC = toPrivate(fun(x => mult(x, stepDivCap))) $ pInc

          toGlobal(id) o
            toPrivate(fun(x => calculateHotspot(x, cc, tInN, cn, tInS, cs, tInE, ce, tInW, cw, tInT, ct, tInB, cb, stepDivCap, pInc, amb_temp))) $ tInC
        })))
        ) $ Zip3D(power, Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) o Pad3D(1, 1, 1, Pad.Boundary.MirrorUnsafe) $ temp)
      })


    val newLambda = SimplifyAndFuse(rodiniaHotSpot3D)
//    val source = Compile(newLambda)
    val source = Compile(newLambda, 32, 4, 2, 512, 512, 8, immutable.Map())

    println(source)
    val (output: Array[Float], runtime) = Execute(2,2,2,2,2,2, (true,true))(source,newLambda, tempInput, powerInput, HotSpotConstants.ce,HotSpotConstants.cw,HotSpotConstants.cn,HotSpotConstants.cs,HotSpotConstants.ct,HotSpotConstants.cb,HotSpotConstants.cc,HotSpotConstants.stepDivCap)

    /*
    if(StencilUtilities.printOutput)
    {
      StencilUtilities.printOriginalAndOutput3DSame(tempInput, output)
    }

    assertArrayEquals(HotSpotConstants.compareDataHotspot3D, output, 0.1f)
        */
  }
  @Ignore
  @Test
  def RodiniaSRAD1(): Unit = {
    val dir = "../../src/test/opencl/generator/stencil/resources/" // should be a better option

    val Nrows = 502
    val Ncols = 458

    val imageBeforeFile = dir + "imagebefore.txt"
    val imageValues: Array[Float] = Source.fromFile(imageBeforeFile).getLines().toList.map(x => x.toFloat).toArray
    //val imageValues2D = imageValues.sliding(Ncols,Ncols).toArray
    // the benchmark is indexed by colum, so we have to go through this rigamarole
    val imageValues2D1 = imageValues.sliding(Nrows, Nrows).toArray
    val imageValues2D = imageValues2D1.transpose

    val coeffBeforeSecondFile = dir + "cbeforesecond.txt"
    val coefficientValues: Array[Float] = Source.fromFile(coeffBeforeSecondFile).getLines().toList.map(x => x.toFloat).toArray

    val m = SizeVar("M")
    val n = SizeVar("N")

    val calculateG2 = UserFun("calculateG2", Array("dN", "dS", "dE", "dW", "jC"),
      "{ return  (dN*dN + dS*dS + dW*dW + dE*dE) / (jC * jC); }", Seq(Float, Float, Float, Float, Float), Float)

    val calculateL = UserFun("calculateL", Array("dN", "dS", "dE", "dW", "jC"),
      "{ return  (dN + dS + dW + dE ) / jC; }", Seq(Float, Float, Float, Float, Float), Float)

    val calculateNum = UserFun("calculateNum", Array("g2", "l"),
      "{ return (0.5*g2) - ((1.0/16.0)*(l*l)); }", Seq(Float, Float), Float)

    val calculateDen = UserFun("calculateDen", Array("L"),
      "{ return 1 + (0.25*L) ; }", Seq(Float), Float)

    val calculateQsqr = UserFun("calculateQsqr", Array("num", "den"),
      "{ return  num/(den*den); }", Seq(Float, Float), Float)

    val calculateDen2 = UserFun("calculateDen2", Array("qsqr", "q0sqr"),
      "{ return (qsqr-q0sqr) / (q0sqr * (1+q0sqr)); }", Seq(Float, Float), Float)

    val calculateCoeff = UserFun("calculateCoeff", Array("den"),
      "{ return 1.0 / (1.0+den); }", Seq(Float), Float)

    val saturateCoeff = UserFun("saturateCoeff", Array("coeff"),
      "{ if(coeff > 1) { return 1.0f; } else if(coeff < 0 ) { return 0.0f; } else { return coeff;} }", Seq(Float), Float)

    val idxF = UserFun("idxF", Array("i", "j", "m", "n"), "{ return i+502*j; }",
      Seq(Int, Int, Int, Int), Int)

    val at = ArrayType(ArrayType(Int, n), m)

    val sradKernel1 = fun(
      ArrayType(ArrayType(Float, m), n),
      (image) => {
        MapGlb(1)(MapGlb(0)(fun((m) => {

          val q0sqr = 0.053787220269f // this value is dependent on data set size !!

          val Jc = m.at(1).at(1)

          val JW = m.at(1).at(0)
          val JN = m.at(0).at(1)
          val JS = m.at(2).at(1)
          val JE = m.at(1).at(2)

          val DW = toPrivate(fun(x => subtract(x, Jc))) $ JW
          val DN = toPrivate(fun(x => subtract(x, Jc))) $ JN
          val DS = toPrivate(fun(x => subtract(x, Jc))) $ JS
          val DE = toPrivate(fun(x => subtract(x, Jc))) $ JE
          val G2 = fun(x => calculateG2(x, DS, DW, DE, Jc)) $ DN
          val L = fun(x => calculateL(x, DS, DW, DE, Jc)) $ DN
          val Num = fun(x => calculateNum(x, L)) $ G2
          val Den = fun(x => calculateDen(x)) $ L
          val Qsqr = fun(x => calculateQsqr(x, Den)) $ Num
          val Den2 = fun(x => calculateDen2(x, q0sqr)) $ Qsqr
          val Coeff = fun(x => calculateCoeff(x)) $ Den2
          val SCoeff = toPrivate(fun(x => saturateCoeff(x))) $ Coeff

          //          val getIdx = Array2DFromUserFunGenerator(idxF, ArrayType(ArrayType(Float, m),n))

          toGlobal(id) $ SCoeff
        }))
        ) o Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.MirrorUnsafe) $ image
      })

    val newLambda = SimplifyAndFuse(sradKernel1)
    val source = Compile(newLambda)
    //println(source)


    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (true, true))(source, newLambda, imageValues2D)

    // undo the transpose
    val outputRemixed = output.sliding(Ncols, Ncols).toArray
    val outputRemixed2 = outputRemixed.transpose

    if (StencilUtilities.printOutput) {
      StencilUtilities.printOriginalAndOutput2D(imageValues2D, outputRemixed2.flatten, Nrows)
    }
    // println("*** EXPECTED ***")
    //StencilUtilities.print2DArray(coefficientValues2D)
    //StencilUtilities.print1DArrayAs2DArray(coefficientValues,Nrows)
    assertArrayEquals(coefficientValues, outputRemixed2.flatten, StencilUtilities.stencilDelta)

  }


  @Ignore
  @Test
  def RodiniaSRAD1Consolidated(): Unit = {
    val dir = "../../src/test/opencl/generator/stencil/resources/" // should be a better option

    val Nrows = 502
    val Ncols = 458

    val imageBeforeFile = dir + "imagebefore.txt"
    val imageValues: Array[Float] = Source.fromFile(imageBeforeFile).getLines().toList.map(x => x.toFloat).toArray
    //val imageValues2D = imageValues.sliding(Ncols,Ncols).toArray
    // the benchmark is indexed by colum, so we have to go through this rigamarole
    val imageValues2D1 = imageValues.sliding(Nrows, Nrows).toArray
    val imageValues2D = imageValues2D1.transpose

    val coeffBeforeSecondFile = dir + "cbeforesecond.txt"
    val coefficientValues: Array[Float] = Source.fromFile(coeffBeforeSecondFile).getLines().toList.map(x => x.toFloat).toArray

    val m = SizeVar("M")
    val n = SizeVar("N")

    val q0sqr = 0.053787220269f // this value is dependent on data set size !!

    val calculateScoeff = UserFun("calculateScoeff", Array("dN", "dS", "dE", "dW", "jC", "q0sqr"),
      "{ float g2 = (dN*dN + dS*dS + dW*dW + dE*dE) / (jC * jC);" +
        "float l = (dN + dS + dW + dE ) / jC; " +
        "float num = (0.5*g2) - ((1.0/16.0)*(l*l)); " +
        "float  den = 1 + (0.25*l);" +
        "float qsqr = num/(den*den); " +
        "den = (qsqr-q0sqr) / (q0sqr * (1+q0sqr));" +
        "float coeff = 1.0 / (1.0+den); " +
        " if(coeff > 1) { return 1.0f; } else if(coeff < 0 ) { return 0.0f; } else { return coeff;}  }", Seq(Float, Float, Float, Float, Float, Float), Float)

    val sradKernel1 = fun(
      ArrayType(ArrayType(Float, m), n),
      Float,
      (image, q0sqr) => {
        MapGlb(1)(MapGlb(0)(fun((m) => {


          val Jc = m.at(1).at(1)

          val JW = m.at(1).at(0)
          val JN = m.at(0).at(1)
          val JS = m.at(2).at(1)
          val JE = m.at(1).at(2)

          val DW = toPrivate(fun(x => subtract(x, Jc))) $ JW
          val DN = toPrivate(fun(x => subtract(x, Jc))) $ JN
          val DS = toPrivate(fun(x => subtract(x, Jc))) $ JS
          val DE = toPrivate(fun(x => subtract(x, Jc))) $ JE

          val scoeff = toPrivate(fun(x => calculateScoeff(x, DS, DE, DW, Jc, q0sqr))) $ DN

          toGlobal(id) $ scoeff
        }))
        ) o Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.MirrorUnsafe) $ image
      })

    val newLambda = SimplifyAndFuse(sradKernel1)
    val source = Compile(newLambda)
    println(source)

    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (true, true))(source, newLambda, imageValues2D,q0sqr)

    // undo the transpose
    val outputRemixed = output.sliding(Ncols, Ncols).toArray
    val outputRemixed2 = outputRemixed.transpose

    if (StencilUtilities.printOutput) {
      StencilUtilities.printOriginalAndOutput2D(imageValues2D, outputRemixed2.flatten, Nrows)
    }
    // println("*** EXPECTED ***")
    //StencilUtilities.print2DArray(coefficientValues2D)
    //StencilUtilities.print1DArrayAs2DArray(coefficientValues,Nrows)
    assertArrayEquals(coefficientValues, outputRemixed2.flatten, StencilUtilities.stencilDelta)
  }

  @Ignore
  @Test
  def RodiniaSRAD2CalculateOnTheFly(): Unit = {
    val dir = "../../src/test/opencl/generator/stencil/resources/" // should be a better option

    val Nrows = 502
    val Ncols = 458

    val imageBeforeFile = dir + "imagebefore.txt"
    val imageValues: Array[Float] = Source.fromFile(imageBeforeFile).getLines().toList.map(x => x.toFloat).toArray
    // val imageValues2D = imageValues.sliding(Ncols,Ncols).toArray
    val imageValues2D1 = imageValues.sliding(Nrows, Nrows).toArray
    val imageValues2D = imageValues2D1.transpose

    val coeffBeforeSecondFile = dir + "cbeforesecond.txt"
    val coefficientValues: Array[Float] = Source.fromFile(coeffBeforeSecondFile).getLines().toList.map(x => x.toFloat).toArray
    //val coeffValues2D = coefficientValues.sliding(Ncols,Ncols).toArray
    val coeffValues2D1 = coefficientValues.sliding(Nrows, Nrows).toArray
    val coeffValues2D = coeffValues2D1.transpose

    val finalImageCompareFile = dir + "imagecompare.txt"
    val finalImageCompareValues: Array[Float] = Source.fromFile(finalImageCompareFile).getLines().toList.map(x => x.toFloat).toArray
    val finalImageValues2D = finalImageCompareValues.sliding(Ncols, Ncols).toArray

    val m = SizeVar("M")
    val n = SizeVar("N")

    val calculateDiv = UserFun("calculateDiv", Array("dN", "dS", "dW", "dE", "orgDn", "orgDs", "orgDw", "orgDe"),
      "{ return  (dN*orgDn + dS*orgDs + dW*orgDw + dE*orgDe) ; }", Seq(Float, Float, Float, Float, Float, Float, Float, Float), Float)

    val calculateImageUpdate = UserFun("calculateImageUpdate", Array("img", "div"),
      "{ return img + 0.125 * div; }", Seq(Float, Float), Float)

    val sradKernel2 = fun(
      ArrayType(ArrayType(Float, m), n),
      ArrayType(ArrayType(Float, m), n),
      (image, coeff) => {
        MapGlb(1)(MapGlb(0)(fun((m) => {

          val imageNBH = Get(m, 0)
          val coeffNBH = Get(m, 1)

          val imageC = imageNBH.at(1).at(1)
          val coeffC = coeffNBH.at(1).at(1)

          val JW = imageNBH.at(1).at(0)
          val JN = imageNBH.at(0).at(1)
          val JS = imageNBH.at(2).at(1)
          val JE = imageNBH.at(1).at(2)

          val orgDW = toPrivate(fun(x => subtract(x, imageC))) $ JW
          val orgDN = toPrivate(fun(x => subtract(x, imageC))) $ JN
          val orgDS = toPrivate(fun(x => subtract(x, imageC))) $ JS
          val orgDE = toPrivate(fun(x => subtract(x, imageC))) $ JE

          val newDW = coeffC
          val newDN = coeffC
          val newDS = coeffNBH.at(2).at(1)
          val newDE = coeffNBH.at(1).at(2)

          val div = toPrivate(fun(x => calculateDiv(x, newDS, newDW, newDE, orgDN, orgDS, orgDW, orgDE))) $ newDN
          val newImg = toPrivate(fun(x => calculateImageUpdate(x, div))) $ imageC

          toGlobal(id) $ newImg
        }))
        ) $ zip2d(Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.MirrorUnsafe) $ image, Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.MirrorUnsafe) $ coeff)
      })

    val newLambda = SimplifyAndFuse(sradKernel2)
    val source = Compile(newLambda)
    //  println(source)

    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (true, true))(source, newLambda, imageValues2D, coeffValues2D)
    val outputRemixed = output.sliding(Ncols, Ncols).toArray
    val outputRemixed2 = outputRemixed.transpose

    if (StencilUtilities.printOutput) {
      StencilUtilities.print2DArray(coeffValues2D)
      println("**** OUTPUT ******")
      StencilUtilities.print2DArray(outputRemixed2)
      println("*** EXPECTED ***")
      StencilUtilities.print2DArray(finalImageValues2D)
    }
    assertArrayEquals(finalImageCompareValues, outputRemixed2.flatten, StencilUtilities.stencilDelta)

  }

  @Ignore
  @Test
  def RodiniaSRAD2CalculatePullInValues(): Unit = {
    val dir = "../../src/test/opencl/generator/stencil/resources/" // should be a better option

    val Nrows = 502
    val Ncols = 458

    val imageBeforeFile = dir + "imagebefore.txt"
    val imageValues: Array[Float] = Source.fromFile(imageBeforeFile).getLines().toList.map(x => x.toFloat).toArray
    // val imageValues2D = imageValues.sliding(Ncols,Ncols).toArray
    val imageValues2D1 = imageValues.sliding(Nrows, Nrows).toArray
    val imageValues2D = imageValues2D1.transpose

    val coeffBeforeSecondFile = dir + "cbeforesecond.txt"
    val coefficientValues: Array[Float] = Source.fromFile(coeffBeforeSecondFile).getLines().toList.map(x => x.toFloat).toArray
    //val coeffValues2D = coefficientValues.sliding(Ncols,Ncols).toArray
    val coeffValues2D1 = coefficientValues.sliding(Nrows, Nrows).toArray
    val coeffValues2D = coeffValues2D1.transpose

    val dDNFile = dir + "dDN.txt"
    val dDNValues: Array[Float] = Source.fromFile(dDNFile).getLines().toList.map(x => x.toFloat).toArray
    val DNValues2D1 = dDNValues.sliding(Nrows, Nrows).toArray
    val DNValues2D = DNValues2D1.transpose

    val dDSFile = dir + "dDS.txt"
    val dDSValues: Array[Float] = Source.fromFile(dDSFile).getLines().toList.map(x => x.toFloat).toArray
    val DSValues2D1 = dDSValues.sliding(Nrows, Nrows).toArray
    val DSValues2D = DSValues2D1.transpose

    val dDEFile = dir + "dDE.txt"
    val dDEValues: Array[Float] = Source.fromFile(dDEFile).getLines().toList.map(x => x.toFloat).toArray
    val DEValues2D1 = dDEValues.sliding(Nrows, Nrows).toArray
    val DEValues2D = DEValues2D1.transpose

    val dDWFile = dir + "dDW.txt"
    val dDWValues: Array[Float] = Source.fromFile(dDWFile).getLines().toList.map(x => x.toFloat).toArray
    val DWValues2D1 = dDWValues.sliding(Nrows, Nrows).toArray
    val DWValues2D = DWValues2D1.transpose

    val finalImageCompareFile = dir + "imagecompare.txt"
    val finalImageCompareValues: Array[Float] = Source.fromFile(finalImageCompareFile).getLines().toList.map(x => x.toFloat).toArray
    val finalImageValues2D = finalImageCompareValues.sliding(Ncols, Ncols).toArray

    val m = SizeVar("M")
    val n = SizeVar("N")

    val calculateDiv = UserFun("calculateDiv", Array("dN", "dS", "dW", "dE", "orgDn", "orgDs", "orgDw", "orgDe"),
      "{ return  (dN*orgDn + dS*orgDs + dW*orgDw + dE*orgDe) ; }", Seq(Float, Float, Float, Float, Float, Float, Float, Float), Float)

    val calculateImageUpdate = UserFun("calculateImageUpdate", Array("img", "div"),
      "{ return img + 0.125 * div; }", Seq(Float, Float), Float)

    val sradKernel2 = fun(
      ArrayType(ArrayType(Float, m), n),
      ArrayType(ArrayType(Float, m), n),
      ArrayType(ArrayType(Float, m), n),
      ArrayType(ArrayType(Float, m), n),
      ArrayType(ArrayType(Float, m), n),
      ArrayType(ArrayType(Float, m), n),
      (image, coeff, DN, DS, DE, DW) => {
        MapGlb(1)(MapGlb(0)(fun((m) => {

          val imageNBH = Get(m, 0)
          val coeffNBH = Get(m, 1)

          val imageC = imageNBH.at(1).at(1)
          val coeffC = coeffNBH.at(1).at(1)

          val newDW = coeffC
          val newDN = coeffC
          val newDS = coeffNBH.at(2).at(1)
          val newDE = coeffNBH.at(1).at(2)

          val orgDN = Get(m, 2)
          val orgDS = Get(m, 3)
          val orgDE = Get(m, 4)
          val orgDW = Get(m, 5)

          val div = toPrivate(fun(x => calculateDiv(x, newDS, newDW, newDE, orgDN, orgDS, orgDW, orgDE))) $ newDN
          val newImg = toPrivate(fun(x => calculateImageUpdate(x, div))) $ imageC

          toGlobal(id) $ newImg
        }))
        ) $ zip2d6(Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.MirrorUnsafe) $ image, Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.MirrorUnsafe) $ coeff, DN, DS, DE, DW)
      })

    val newLambda = SimplifyAndFuse(sradKernel2)
    val source = Compile(newLambda)
    //  println(source)

    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (true, true))(source, newLambda, imageValues2D, coeffValues2D, DNValues2D, DSValues2D, DEValues2D, DWValues2D)
    val outputRemixed = output.sliding(Ncols, Ncols).toArray
    val outputRemixed2 = outputRemixed.transpose

    if (StencilUtilities.printOutput) {
      StencilUtilities.print2DArray(coeffValues2D)
      println("**** OUTPUT ******")
      StencilUtilities.print2DArray(outputRemixed2)
      println("*** EXPECTED ***")
      StencilUtilities.print2DArray(finalImageValues2D)
    }
    assertArrayEquals(finalImageCompareValues, outputRemixed2.flatten, .002f)

  }

}

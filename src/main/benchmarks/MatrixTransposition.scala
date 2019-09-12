package benchmarks

import lift.arithmetic.SizeVar
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

@deprecated("Uses an old benchmark infrastructure", "")
class MatrixTransposition (override val f: Seq[(String, Array[Lambda])])
  extends DeprecatedBenchmark("Matrix Transposition", Seq(1024, 1024), f, 0.0f, Array(16, 16, 1)) {

  val defaultTileSize = 16

  case class MatrixTranspConfig(tileX: Option[Long] = None,
                                tileY: Option[Long] = None)


  var matrixTranspCmdArgs = MatrixTranspConfig()
  val matrixTranspParser = new scopt.OptionParser[Unit]("MatrixTransposition") {
    override val errorOnUnknownArgument = false

    opt[Long]('x', "tileX").text("Tile size in the M and N dimension").required()
      .foreach(arg => matrixTranspCmdArgs = matrixTranspCmdArgs.copy(tileX = Some(arg)))

    opt[Long]('y', "tileY").text("Tile size in the K dimension").required()
      .foreach(arg => matrixTranspCmdArgs = matrixTranspCmdArgs.copy(tileY = Some(arg)))
  }


  override def runScala(inputs: Any*): Array[Float] = {
    val matrix = inputs(0).asInstanceOf[Array[Array[Float]]]

    matrix.transpose.flatten
  }

  override def generateInputs(): Seq[Any] = {
    val inputSizeN = inputSizes()(0)
    val inputSizeM = inputSizes()(1)

    val matrix = Array.tabulate(inputSizeN, inputSizeM)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)

    Seq(matrix)
  }

  override def globalSize: Array[Int] = {
    val globalSizes = Array(inputSizes()(0), inputSizes()(1), 1)
    cmdArgs.globalSize.copyToArray(globalSizes)
    globalSizes
  }

  override protected def beforeBenchmark(): Unit = {
    val temp = f(1)._2
    temp(0) = MatrixTransposition.coalesced(matrixTranspCmdArgs.tileX.getOrElse(defaultTileSize),
      matrixTranspCmdArgs.tileY.getOrElse(defaultTileSize))
  }

  override protected def printParams(): Unit = {
    if (variant == 1)
      println("Tile sizes: " + matrixTranspCmdArgs.tileX.getOrElse(defaultTileSize) +
        ", " + matrixTranspCmdArgs.tileY.getOrElse(defaultTileSize))
  }
}

object MatrixTransposition {
  val N = SizeVar("N")
  val M = SizeVar("M")

  val naive = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    (matrix) => {
      MapGlb(0)(MapGlb(1)(id)) o Transpose() $ matrix
    })

  def coalesced(x: Long = 4, y: Long = 4) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    (matrix) => {
      // Merge the tiles
      Untile2D() o
      MapWrg(0)(MapWrg(1)(
         toGlobal(MapLcl(1)(MapLcl(0)(id))) o
        // Transpose the tiles and then the insides of tiles
        TransposeW() o  toLocal(MapLcl(1)(MapLcl(0)(id)))
      )) o Transpose() o
        // Tile the matrix
        Tile(x, y) $ matrix
    })

  def apply() = new MatrixTransposition(
    Seq(("naive", Array[Lambda](naive)),
      ("coalesced", Array[Lambda](coalesced()))
    ))

  def main(args: Array[String]): Unit = {
    val m = MatrixTransposition()

    if (!m.matrixTranspParser.parse(args))
      throw new IllegalArgumentException("Wrong command line arguments passed")

      m.run(args)
  }
}

import java.nio.file.Files._
import java.nio.file.Paths._
import java.nio.file.{Files, Paths}
import java.nio.{ByteBuffer, ByteOrder}
import java.util.Calendar

import ir.ArrayType
import ir.ast.UserFun
import opencl.executor.Executor
import opencl.ir._
import patterns.nn._

/**
  * Created by s1569687 on 28/02/17.
  */
package object nn {

  /* Types and data structures */

  case class Shape(size: Int = 0,
                   var sizePadded: Int = 0,
                   nChannels: Int = 0,
                   nInputs: Int = 0,
                   nBatches: Int = 0) {
    override def toString: String =
      f"Shape(" +
        f"\nsize = $size%d, sizePadded = $sizePadded%d, nChannels = $nChannels%d," +
        f"\nnInputs = $nInputs%d, nBatches = $nBatches%d)"}

  trait NetDatasets {
//    var inputs: PaddedArray[_]
//    var outputs: PaddedArray[_]
//    val targets: Array[_]
//    val weights: Any
//    val biases: Any
  }

  case class NetDatasetsCollection(pathToParams: String,
                                   nInputs: Int,
                                   perLayer: Array[NetDatasets])

  case class PaddedArray[T](var nonPadded: T) {
    var padded: T = _
  }

  def AT = ArrayType
  type AT = ArrayType


  /* Variables */
  val localMemSize: Int = Executor.getDeviceLocalMemSize.toInt
  val maxWorkGroupSize: Int = Executor.getDeviceMaxWorkGroupSize.toInt
  val deviceName: String = Executor.getDeviceName


  /* Functions */

  // Activation functions
  val ReLU: UserFun = UserFun("ReLU", "x", "{ return(max(0.0f, x)); }", Float, Float)
  val max: UserFun = UserFun("maxFloat", Array("x", "y"), "return x > y ? x : y;", Seq(Float, Float), Float)
  val Linear: UserFun = id

  /*def loadJSON5D(json_file_path: String): Array5D[Float] = {
    /* Load an array from a JSON file */
    val source = scala.io.Source.fromFile(json_file_path)
    val jsonString = source.getLines mkString "\n"
    source.close()
    val json:Option[Any] = JSON.parseFull(jsonString)
    val w_list: List[List[List[List[List[Double]]]]] =
      json.get.asInstanceOf[List[List[List[List[List[Double]]]]]]

    // Convert from List[List[Double]] to Array2D[Float]
    val w_arr = Array.fill[Array4D[Float]](w_list.length)(
      Array.fill[Array3D[Float]](w_list.head.length)(
        Array.fill[Array2D[Float]](w_list.head.head.length)(
          Array.fill[Array[Float]](w_list.head.head.head.length)(
            Array.fill[Float](w_list.head.head.head.head.length)(0)))))

    var aline = Array.fill[Double](w_list.head.length)(0)
    for (i <- w_list.indices) {
      for (j <- w_list(i).indices) {
        for (k <- w_list(i)(j).indices) {
          for (l <- w_list(i)(j)(k).indices) {
            aline = w_list(i)(j)(k)(l).to[Array]
            for (m <- aline.indices) {
              w_arr(i)(j)(k)(l)(m) = aline(m).toFloat
            }
          }
        }
      }
    }
    w_arr
  }

  def loadJSON4D(json_file_path: String): Array4D[Float] = {
    /* Load an array from a JSON file */
    val source = scala.io.Source.fromFile(json_file_path)
    val jsonString = source.getLines mkString "\n"
    source.close()
    val json:Option[Any] = JSON.parseFull(jsonString)
    val w_list: List[List[List[List[Double]]]] = json.get.asInstanceOf[List[List[List[List[Double]]]]]

    // Convert from List[List[Double]] to Array2D[Float]
    val w_arr = Array.fill[Array3D[Float]](w_list.length)(
      Array.fill[Array2D[Float]](w_list.head.length)(
        Array.fill[Array[Float]](w_list.head.head.length)(
          Array.fill[Float](w_list.head.head.head.length)(0))))

    var aline = Array.fill[Double](w_list.head.length)(0)
    for (i <- w_list.indices) {
      for (j <- w_list(i).indices) {
        for (k <- w_list(i)(j).indices) {
          aline = w_list(i)(j)(k).to[Array]
          for (l <- aline.indices) {
            w_arr(i)(j)(k)(l) = aline(l).toFloat
          }
        }
      }
    }
    w_arr
  }

  def loadJSON3D(json_file_path: String): Array3D[Float] = {
    /* Load an array from a JSON file */
    val source = scala.io.Source.fromFile(json_file_path)
    val jsonString = source.getLines mkString "\n"
    source.close()
    val json:Option[Any] = JSON.parseFull(jsonString)
    val w_list: List[List[List[Double]]] = json.get.asInstanceOf[List[List[List[Double]]]]

    // Convert from List[List[Double]] to Array2D[Float]
    val w_arr = Array.fill[Array2D[Float]](w_list.length)(
      Array.fill[Array[Float]](w_list.head.length)(
        Array.fill[Float](w_list.head.head.length)(0)))
    var aline = Array.fill[Double](w_list.head.length)(0)
    for (i <- w_list.indices) {
      for (j <- w_list(i).indices) {
        aline = w_list(i)(j).to[Array]
        for (k <- aline.indices) {
          w_arr(i)(j)(k) = aline(k).toFloat
        }
      }
    }
    w_arr
  }

  def loadJSON2D(json_file_path: String): Array2D[Float] = {
    /* Load an array from a JSON file */
    val source = scala.io.Source.fromFile(json_file_path)
    val jsonString = source.getLines mkString "\n"
    source.close()
    val json:Option[Any] = JSON.parseFull(jsonString)
    val w_list: List[List[Double]] = json.get.asInstanceOf[List[List[Double]]]

    // Convert from List[List[Double]] to Array2D[Float]
    val w_arr = Array.fill[Array[Float]](w_list.length)(Array.fill[Float](w_list.head.length)(0))
    var aline = Array.fill[Double](w_list.head.length)(0)
    for (i <- w_list.indices) {
      aline = w_list(i).to[Array]
      for (j <- aline.indices) {
        w_arr(i)(j) = aline(j).toFloat
      }
    }
    w_arr
  }

  def loadJSON1D(json_file_path: String): Array[Float] = {
    /* Load an array from a JSON file */
    val source = scala.io.Source.fromFile(json_file_path)
    val jsonString = source.getLines mkString "\n"
    source.close()
    val json:Option[Any] = JSON.parseFull(jsonString)
    val w_list: List[Double] = json.get.asInstanceOf[List[Double]]

    // Convert from List[List[Double]] to Array2D[Float] 
    val w_arr = Array.fill[Float](w_list.length)(0)
    var aline = Array.fill[Double](w_list.length)(0)
    aline = w_list.to[Array]
    for (i <- aline.indices) {
      w_arr(i) = aline(i).toFloat
    }
    w_arr
  }*/


  def loadBinary(filePath: String): Array[Float] = {
    //val bis = new BufferedInputStream(new FileInputStream(filePath))
    val byteArray: Array[Byte] = Files.readAllBytes(Paths.get(filePath))
    val times = 4
    val floats: Array[Float] = new Array[Float](byteArray.length / times)
    var b: ByteBuffer = null
    for (i <- floats.indices) {
      b = ByteBuffer.wrap(byteArray, i * times, times)
      b.order(ByteOrder.LITTLE_ENDIAN)
      floats(i) = b.getFloat
    }
    floats
  }

  def loadBinary(filePath: String, shape: (Int, Int)): Array2D[Float] =
    group(loadBinary(filePath), (shape._1, shape._2))

  def loadBinary(filePath: String, shape: (Int, Int, Int)): Array3D[Float] =
    group(loadBinary(filePath), (shape._1, shape._2, shape._3))

  def loadBinary(filePath: String, shape: (Int, Int, Int, Int)): Array4D[Float] =
    group(loadBinary(filePath), (shape._1, shape._2, shape._3, shape._4))

  def loadBinary(filePath: String, shape: (Int, Int, Int, Int, Int)): Array5D[Float] =
    group(loadBinary(filePath), (shape._1, shape._2, shape._3, shape._4, shape._5))

  var runnerIsConsole: Boolean = false

  val nnDir: String = {
    // Launching from IntelliJ or from console?
    val intellij_path = System.getProperty("user.dir") + "/../../src/test/nn"
    val console_path = System.getProperty("user.dir") + "/src/test/nn"
    if (exists(get(intellij_path)))
      intellij_path
    else {
      runnerIsConsole = true
      console_path
    }
  }

  def resultsFilename(results_dir_name: String, n_inputs: Int): String = {
    val now = Calendar.getInstance()
    new String(results_dir_name +
      "%02d.%02d.%04d-%02d.%02d.%02d.%03d_n%d.csv".format(
        now.get(Calendar.DATE), now.get(Calendar.MONTH), now.get(Calendar.YEAR),
        now.get(Calendar.HOUR_OF_DAY), now.get(Calendar.MINUTE), now.get(Calendar.SECOND),
        now.get(Calendar.MILLISECOND), n_inputs))
  }

  def fill(v: Float, shape: Int): Array[Float] = {
    Array.fill[Float](shape)(v)
  }

  def fill(v: Float, shape: (Int, Int)): Array2D[Float] = {
    Array.fill[Array[Float]](shape._1)(fill(v, shape._2))
  }

  def fill(v: Float, shape: (Int, Int, Int)): Array3D[Float] = {
    Array.fill[Array2D[Float]](shape._1)(fill(v, (shape._2, shape._3)))
  }

  def fill(v: Float, shape: (Int, Int, Int, Int)): Array4D[Float] = {
    Array.fill[Array3D[Float]](shape._1)(fill(v, (shape._2, shape._3, shape._4)))
  }

  def fill(v: Float, shape: (Int, Int, Int, Int, Int)): Array5D[Float] = {
    Array.fill[Array4D[Float]](shape._1)(fill(v, (shape._2, shape._3, shape._4, shape._5)))
  }

  trait Experiment
}

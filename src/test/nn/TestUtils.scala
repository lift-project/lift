package nn

import java.nio.file.Files._
import java.nio.file.Paths._
import java.util.Calendar

import opencl.executor.Executor

import scala.util.parsing.json.JSON

/**
  * Created by nm on 08/02/17.
  */
object TestUtils {
  val localMemSize = Executor.getDeviceLocalMemSize.toInt
  val maxWorkGroupSize = Executor.getDeviceMaxWorkGroupSize.toInt
  val deviceName = Executor.getDeviceName


  def loadJSON4D(json_file_path: String): Array[Array[Array[Array[Float]]]] = {
    /* Load an array from a JSON file */
    val source = scala.io.Source.fromFile(json_file_path)
    val jsonString = source.getLines mkString "\n"
    source.close()
    val json:Option[Any] = JSON.parseFull(jsonString)
    val w_list: List[List[List[List[Double]]]] = json.get.asInstanceOf[List[List[List[List[Double]]]]]

    // Convert from List[List[Double]] to Array[Array[Float]]
    val w_arr = Array.fill[Array[Array[Array[Float]]]](w_list.length)(
      Array.fill[Array[Array[Float]]](w_list.head.length)(
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

  def loadJSON3D(json_file_path: String): Array[Array[Array[Float]]] = {
    /* Load an array from a JSON file */
    val source = scala.io.Source.fromFile(json_file_path)
    val jsonString = source.getLines mkString "\n"
    source.close()
    val json:Option[Any] = JSON.parseFull(jsonString)
    val w_list: List[List[List[Double]]] = json.get.asInstanceOf[List[List[List[Double]]]]

    // Convert from List[List[Double]] to Array[Array[Float]]
    val w_arr = Array.fill[Array[Array[Float]]](w_list.length)(
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

  def loadJSON2D(json_file_path: String): Array[Array[Float]] = {
    /* Load an array from a JSON file */
    val source = scala.io.Source.fromFile(json_file_path)
    val jsonString = source.getLines mkString "\n"
    source.close()
    val json:Option[Any] = JSON.parseFull(jsonString)
    val w_list: List[List[Double]] = json.get.asInstanceOf[List[List[Double]]]

    // Convert from List[List[Double]] to Array[Array[Float]]
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

    // Convert from List[List[Double]] to Array[Array[Float]]
    val w_arr = Array.fill[Float](w_list.length)(0)
    var aline = Array.fill[Double](w_list.length)(0)
    aline = w_list.to[Array]
    for (i <- aline.indices) {
      w_arr(i) = aline(i).toFloat
    }
    w_arr
  }

  var runnerIsConsole: Boolean = false

  val currentDir = {
    // Launching from IntelliJ or from console?
    val intellij_path = System.getProperty("user.dir") + "/../../src/test/nn/mlp"
    val console_path = System.getProperty("user.dir") + "/src/test/nn/mlp"
    if (exists(get(intellij_path)))
      intellij_path
    else {
      runnerIsConsole = true
      console_path
    }
  }

  def resultsFilename(exp_dir_name: String, n_inputs: Int) = {
    val now = Calendar.getInstance()
    new String(exp_dir_name + "/results_lift/" +
      "%02d.%02d.%04d-%02d.%02d.%02d.%03d_n%d.csv".format(
        now.get(Calendar.DATE), now.get(Calendar.MONTH), now.get(Calendar.YEAR),
        now.get(Calendar.HOUR_OF_DAY), now.get(Calendar.MINUTE), now.get(Calendar.SECOND),
        now.get(Calendar.MILLISECOND), n_inputs))
  }

  def group(arr1d: Array[Float], shape: (Int, Int)) = {
    val arr2d = Array.fill[Array[Float]](shape._1)(
      Array.fill[Float](shape._2)(0))
    for (i <- 0 until shape._1; j <- 0 until shape._2) {
      arr2d(i)(j) = arr1d(i * shape._2 + j)
    }
    arr2d
  }

  def group(arr1d: Array[Float], shape: (Int, Int, Int)) = {
    val arr3d = Array.fill[Array[Array[Float]]](shape._1)(
      Array.fill[Array[Float]](shape._2)(
        Array.fill[Float](shape._3)(0)))
    for (i <- 0 until shape._1; j <- 0 until shape._2; k <- 0 until shape._3) {
      arr3d(i)(j)(k) = arr1d(i * shape._2 * shape._3 + j * shape._3 + k)
    }
    arr3d
  }
}

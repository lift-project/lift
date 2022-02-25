package benchmarks.conv.gpgpu20

import lift.arithmetic.ArithExpr

import java.io.File
import scala.sys.process.{Process, ProcessLogger}

package object utils {
  def log2(num: Int): Int = (Math.log(num) / Math.log(2)).toInt

  def powersOf2To(upperLimit: Int): Seq[Int] =
    for (x <- 0 to log2(upperLimit)) yield Math.pow(2, x.toDouble).toInt


  def joinPaths(str0: String, str1: String): String =
    new File(new File(str0), str1).getAbsolutePath

  def joinPaths(str0: String, str1: String, str2: String): String =
    new File(new File(joinPaths(str0, str1)), str2).getAbsolutePath

  def joinPaths(str0: String, str1: String, str2: String, str3: String): String =
    new File(new File(joinPaths(str0, str1, str2)), str3).getAbsolutePath


  //noinspection ConvertibleToMethodValue
  def runShell(cmd: Seq[String]): (String, String) = {
    val process = Process(cmd, new File(System.getProperty("user.dir")))
    val (out, err) = (new StringBuffer(), new StringBuffer())
    val logger = ProcessLogger(s => out.append(s + "\n"),
      s => err.append(s + "\n"))

    process.!(logger)

    (out.toString.trim, err.toString.trim)
  }

  def log(value: ArithExpr, base: ArithExpr): Int = (Math.log(value.evalInt) / Math.log(base.evalInt)).toInt

  // TODO: overload
  def shape1d(seq: Array[Float]): List[Int] = {
    List(seq.size)
  }

  def shape2d(seq: Array[Array[Float]]): List[Int] = {
    List(seq.size) ++ shape1d(seq.head)
  }

  def shape3d(seq: Array[Array[Array[Float]]]): List[Int] = {
    List(seq.size) ++ shape2d(seq.head)
  }

  def shape4d(seq: Array[Array[Array[Array[Float]]]]): List[Int] = {
    List(seq.size) ++ shape3d(seq.head)
  }

  def shape5d(seq: Array[Array[Array[Array[Array[Float]]]]]): List[Int] = {
    List(seq.size) ++ shape4d(seq.head)
  }

  def shape6d(seq: Array[Array[Array[Array[Array[Array[Float]]]]]]): List[Int] = {
    List(seq.size) ++ shape5d(seq.head)
  }
}

package benchmarks.conv

import java.io.File

import com.typesafe.scalalogging.Logger
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{Reads, _}
import ConvExplorationJSONSettings.SettingValueCaster._

import scala.io.Source

/**
 * Setting storage classes and corresponding JSON parsers.
 * Based on exploration.ParametersAndSettings
 */
case class ConvExplorationJSONSettings(maxLocalSize0: Int,
                                       maxLocalSize1: Int,
                                       maxLocalSize2: Int,
                                       maxWorkgroupSize: Int,
                                       lambdaDirRoot: String,
                                       randomSeed: Int,
                                       testSrcDir: String,
                                       testHarnessDirName: String,
                                       testHarnessFilesCopyScriptFName: String,
                                       generatedHostCodeFName: String,
                                       testHarnessFName: String,
                                       floatRegistersPerThreadInMaxOccupancy: Long,
                                       maxMemAllocSize: Long,
                                       globalMemSize: Long,
                                       localMemSize: Long) {
  def maxLocalSize(dim: Int): Int = dim match {
    case 0 => maxLocalSize0
    case 1 => maxLocalSize1
    case 2 => maxLocalSize2
    case _ => throw new IllegalArgumentException
  }

    override def toString: String =
      s"""maxLocalSize0 = $maxLocalSize0 ; maxLocalSize1 = $maxLocalSize1 ; maxLocalSize2 = $maxLocalSize2
         |maxWorkgroupSize = $maxWorkgroupSize
         |lambdaDirRoot = $lambdaDirRoot
         |randomSeed = $randomSeed
         |testSrcDir = $testSrcDir
         |testHarnessDirName = $testHarnessDirName
         |testHarnessFilesCopyScriptFName = $testHarnessFilesCopyScriptFName
         |generatedHostCodeFName = $generatedHostCodeFName
         |testHarnessFName = $testHarnessFName
         |floatRegistersPerThreadInMaxOccupancy = $floatRegistersPerThreadInMaxOccupancy
         |maxMemAllocSize = $maxMemAllocSize
         |globalMemSize = $globalMemSize
         |localMemSize = $localMemSize""".stripMargin
}


object ConvExplorationJSONSettings {
  private val logger = Logger(this.getClass)


  case class Setting[T](name: String,
                        defaultValue: Option[T] = None) {
    def read(implicit caster: SettingValueCaster[T]): Reads[T] = defaultValue match {
      case Some(v)  => caster.read(JsPath \ name, v)
      case None     => caster.read(JsPath \ name)
    }
  }

  trait SettingValueCaster[T] {
    def read(jsPath: JsPath, defaultValue: T): Reads[T]
    def read(jsPath: JsPath): Reads[T]
  }

  object SettingValueCaster {

    implicit object StringSettingValueCaster extends SettingValueCaster[String] {
      def read(jsPath: JsPath, defaultValue: String): Reads[String] = jsPath.readWithDefault[String](defaultValue)
      def read(jsPath: JsPath): Reads[String]                       = jsPath.read[String]
    }

    implicit object OptStringSettingValueCaster extends SettingValueCaster[Option[String]] {
      def read(jsPath: JsPath, defaultValue: Option[String]): Reads[Option[String]] =
        throw new NoSuchMethodError()

      def read(jsPath: JsPath): Reads[Option[String]] = jsPath.readNullable[String]
    }

    implicit object IntSettingValueCaster extends SettingValueCaster[Int] {
      def read(jsPath: JsPath, defaultValue: Int): Reads[Int] = jsPath.readWithDefault[Int](defaultValue)
      def read(jsPath: JsPath): Reads[Int]                    = jsPath.read[Int]
    }

    implicit object LongSettingValueCaster extends SettingValueCaster[Long] {
      def read(jsPath: JsPath, defaultValue: Long): Reads[Long] = jsPath.readWithDefault[Long](defaultValue)
      def read(jsPath: JsPath): Reads[Long]                     = jsPath.read[Long]
    }
  }

  val maxLocalSize0: Setting[Int] = Setting("maxLocalSize0")
  val maxLocalSize1: Setting[Int] = Setting("maxLocalSize1")
  val maxLocalSize2: Setting[Int] = Setting("maxLocalSize2")
  val maxWorkgroupSize: Setting[Int] = Setting("maxWorkgroupSize")
  val lambdaDirRoot: Setting[String] = Setting("lambdaDirRoot")
  val randomSeed: Setting[Int] = Setting("randomSeed", defaultValue = Some(0))
  val solverStateBackupsDirRoot: Setting[Option[String]] = Setting("solverStateBackupsDirRoot")
  val solverStateRestoreDir: Setting[Option[String]] = Setting("solverStateRestoreDir")
  val testSrcDir: Setting[String] = Setting("testSrcDir")
  val testHarnessDirName: Setting[String] = Setting("testHarnessDirName")
  val testHarnessFilesCopyScriptFName: Setting[String] = Setting("testHarnessFilesCopyScriptFName")
  val generatedHostCodeFName: Setting[String] = Setting("generatedHostCodeFName")
  val testHarnessFName: Setting[String] = Setting("testHarnessFName")
  val floatRegistersPerThreadInMaxOccupancy: Setting[Long] = Setting("floatRegistersPerThreadInMaxOccupancy")
  val maxMemAllocSize: Setting[Long] = Setting("maxMemAllocSize")
  val globalMemSize: Setting[Long] = Setting("globalMemSize")
  val localMemSize: Setting[Long] = Setting("localMemSize")

  private implicit val settingsReads: Reads[ConvExplorationJSONSettings] = {
    (maxLocalSize0.read and
      maxLocalSize1.read and
      maxLocalSize2.read and
      maxWorkgroupSize.read and
      lambdaDirRoot.read and
      randomSeed.read and
      testSrcDir.read and
      testHarnessDirName.read and
      testHarnessFilesCopyScriptFName.read and
      generatedHostCodeFName.read and
      testHarnessFName.read and
      floatRegistersPerThreadInMaxOccupancy.read and
      maxMemAllocSize.read and
      globalMemSize.read and
      localMemSize.read)((p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14) =>
      ConvExplorationJSONSettings(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14))
  }


  /**
   * Parse a JSON settings file.
   * @param filename path to the file
   * @return The settings object
   */
  def apply(filename: String): ConvExplorationJSONSettings = {
    // Check if file exists
    val file = new File(filename)
    if (!file.exists)
      throw new java.io.FileNotFoundException(s"Settings file $filename doesn't exist.")
    if (!file.isFile)
      throw new java.io.FileNotFoundException(s"The following path does not lead to a file: $filename ")

    // Parse the file
    val settingsFile = Source.fromFile(filename)
    val settingsString = settingsFile.mkString
    settingsFile.close()

    val json = Json.parse(settingsString)

    json.validate[ConvExplorationJSONSettings] match {
      case JsSuccess(settings, _) => settings
      case e: JsError =>
        logger.error("Failed to parse settings " +
          e.recoverTotal(e => JsError.toJson(e)))
        sys.exit(1)
    }
  }

}
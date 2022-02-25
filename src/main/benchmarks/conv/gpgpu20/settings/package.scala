package benchmarks.conv.gpgpu20

import java.io.File

import com.typesafe.scalalogging.Logger
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import scala.collection.immutable.ListMap
import scala.io.Source

/**
  * Setting storage classes and corresponding JSON parsers.
  * Based on lift.exploration.ParametersAndSettings
  */
package object settings {


  abstract class ExplorationSettings {
    val defaultParameters : ListMap[String, Any]
  }


  object Settings {
    protected[settings] val pythonExec = "python_exec"
    protected[settings] val lambdaDir = "lambda_dir"
    protected[settings] val kernelsDir = "kernels_dir"
    protected[settings] val onnxConfigsRootDir = "onnx_configs_root_dir"
    protected[settings] val allowOverwritingOnnxConfigs = "allow_overwriting_onnx_configs"
    protected[settings] val enableMySQL = "enable_mysql"
    protected[settings] val mysqlServerHostname = "mysql_server_hostname"
    protected[settings] val mysqlServerPort = "mysql_server_port"
    protected[settings] val mysqlDBName = "mysql_db_name"
    protected[settings] val mysqlUsername = "mysql_username"
    protected[settings] val mysqlPassword = "mysql_password"
    protected[settings] val randomSeed = "random_seed"
    protected[settings] val configPoints = "config_points"
    protected[settings] val optPoints = "opt_points"
    protected[settings] val configSpacesTableNamePrefix = "config_spaces_table_name"
    protected[settings] val tuneParamSpacesTableNamePrefix = "tune_param_spaces_table_name"
    protected[settings] val validate = "validate"
    protected[settings] val maxLocalSize = "maxLocalSize"
    protected[settings] val maxWorkGroupSize = "maxWorkGroupSize"

    protected[settings] val defaultPythonExec = "python3"
    protected[settings] val defaultAllowOverwritingOnnxConfigs = false
    protected[settings] val defaultEnableMySQL = false
    protected[settings] val defaultMysqlServerPort = "3306"
    protected[settings] val defaultRandomSeed = 0
    protected[settings] val defaultConfigSpacesTableNamePrefix = "configSpaces"
    protected[settings] val defaultTuneParamSpacesTableNamePrefix = "optSpaces"

    protected[settings] val defaultParameters: ListMap[String, Any] = ListMap(
      pythonExec -> defaultPythonExec,
      allowOverwritingOnnxConfigs -> defaultAllowOverwritingOnnxConfigs,
      enableMySQL -> defaultEnableMySQL,
      mysqlServerPort -> defaultMysqlServerPort,
      randomSeed -> defaultRandomSeed,
      configSpacesTableNamePrefix -> defaultConfigSpacesTableNamePrefix,
      tuneParamSpacesTableNamePrefix -> defaultTuneParamSpacesTableNamePrefix
    )

    def checkSettings(settings: Settings): Unit = {
      // TODO
    }

    def checkMySQLSettings(settings: Settings): Unit = {
      if (settings.mysqlServerHostname.isEmpty ||
        settings.mysqlDBName.isEmpty ||
        settings.mysqlUsername.isEmpty ||
        settings.mysqlPassword.isEmpty)
        throw new IllegalArgumentException("MySQL settings incomplete. They must include hostname, database nname, " +
          "username and password.")
    }
  }


  case class Settings(pythonExec: String,
                      lambdaDir: String,
                      kernelsDir: String,
                      onnxConfigsRootDir: String,
                      allowOverwritingOnnxConfigs: Boolean,
                      enableMySQL: Boolean,
                      mysqlServerHostname: Option[String],
                      mysqlServerPort: String,
                      mysqlDBName: Option[String],
                      mysqlUsername: Option[String],
                      mysqlPassword: Option[String],
                      configSpacesTableNamePrefix: String,
                      optSpacesTableNamePrefix: String,
                      randomSeed: Int,
                      configPoints: Int,
                      optPoints: Int,
                      validate: Boolean,
                      maxLocalSize: Int,
                      maxWorkGroupSize: Int) extends ExplorationSettings {
    override def toString: String = {
      s"""Settings(
         |  ${Settings.pythonExec}:
         |      $pythonExec
         |  ${Settings.onnxConfigsRootDir}:
         |      $onnxConfigsRootDir
         |  ${Settings.lambdaDir}:
         |      $lambdaDir
         |  ${Settings.kernelsDir}:
         |      $kernelsDir
         |  ${Settings.allowOverwritingOnnxConfigs}:
         |      $allowOverwritingOnnxConfigs
         |  ${Settings.enableMySQL}:
         |      $enableMySQL
         |  ${Settings.mysqlServerHostname}:
         |      $mysqlServerHostname
         |  ${Settings.mysqlServerPort}:
         |      $mysqlServerPort
         |  ${Settings.mysqlDBName}:
         |      $mysqlDBName
         |  ${Settings.mysqlUsername}:
         |      $mysqlUsername
         |  ${Settings.mysqlPassword}:
         |      $mysqlPassword
         |  ${Settings.configSpacesTableNamePrefix}:
         |      $configSpacesTableNamePrefix
         |  ${Settings.tuneParamSpacesTableNamePrefix}:
         |      $optSpacesTableNamePrefix
         |  ${Settings.randomSeed}:
         |      $randomSeed
         |  ${Settings.configPoints}:
         |      $configPoints
         |  ${Settings.optPoints}:
         |      $optPoints
         |  ${Settings.validate}:
         |      $validate
         |  ${Settings.maxLocalSize}:
         |      $maxLocalSize
         |  ${Settings.maxWorkGroupSize}:
         |      $maxWorkGroupSize
         |)""".stripMargin
    }

    override val defaultParameters: ListMap[String, Any] = Settings.defaultParameters
  }


  object ParseSettings {
    private val logger = Logger(this.getClass)

    private[settings] implicit val settingsReads: Reads[Settings] = (
      (JsPath \ Settings.pythonExec).readNullable[String] and
        (JsPath \ Settings.lambdaDir).read[String] and
        (JsPath \ Settings.kernelsDir).read[String] and
        (JsPath \ Settings.onnxConfigsRootDir).read[String] and
        (JsPath \ Settings.allowOverwritingOnnxConfigs).readNullable[Boolean] and
        (JsPath \ Settings.enableMySQL).readNullable[Boolean] and
        (JsPath \ Settings.mysqlServerHostname).readNullable[String] and
        (JsPath \ Settings.mysqlServerPort).readNullable[String] and
        (JsPath \ Settings.mysqlDBName).readNullable[String] and
        (JsPath \ Settings.mysqlUsername).readNullable[String] and
        (JsPath \ Settings.mysqlPassword).readNullable[String] and
        (JsPath \ Settings.configSpacesTableNamePrefix).readNullable[String] and
        (JsPath \ Settings.tuneParamSpacesTableNamePrefix).readNullable[String] and
        (JsPath \ Settings.randomSeed).readNullable[Int] and
        (JsPath \ Settings.configPoints).read[Int] and
        (JsPath \ Settings.optPoints).read[Int] and
        (JsPath \ Settings.validate).read[Boolean] and
        (JsPath \ Settings.maxLocalSize).read[Int] and
        (JsPath \ Settings.maxWorkGroupSize).read[Int])(
      (pythonExec,
       lambdaDir,
       kernelsDir,
       onnxConfigsRootDir,
       allowOverwritingOnnxConfigs,
       enableMySQL,
       mysqlServerHostname,
       mysqlServerPort,
       mysqlDBName,
       mysqlUsername,
       mysqlPassword,
       configSpacesTableNamePrefix,
       optSpacesTableNamePrefix,
       randomSeed,
       configPoints,
       optPoints,
       validate,
       maxLocalSize,
       maxWorkGroupSize) =>
        Settings(
          pythonExec.getOrElse(Settings.defaultPythonExec),
          lambdaDir,
          kernelsDir,
          onnxConfigsRootDir,
          allowOverwritingOnnxConfigs.getOrElse(Settings.defaultAllowOverwritingOnnxConfigs),
          enableMySQL.getOrElse(Settings.defaultEnableMySQL),
          mysqlServerHostname,
          mysqlServerPort.getOrElse(Settings.defaultMysqlServerPort),
          mysqlDBName,
          mysqlUsername,
          mysqlPassword,
          configSpacesTableNamePrefix.getOrElse(Settings.defaultConfigSpacesTableNamePrefix),
          optSpacesTableNamePrefix.getOrElse(Settings.defaultTuneParamSpacesTableNamePrefix),
          randomSeed.getOrElse(Settings.defaultRandomSeed),
          configPoints,
          optPoints,
          validate,
          maxLocalSize,
          maxWorkGroupSize))

    def apply(filename: String): Settings = {
      // Check if file exists
      val file = new File(filename)
      if (!file.exists)
        throw new java.io.FileNotFoundException(s"Settings file $filename doesn't exist.")
      if (!file.isFile)
        throw new java.io.FileNotFoundException(s"The following path does not lead to a file: $filename ")

      // Parse the file
      val settingsString = Source.fromFile(filename).mkString
      val json = Json.parse(settingsString)
      def validated = json.validate[Settings]

      validated match {
        case JsSuccess(settings, _) =>
          Settings.checkSettings(settings)
          settings
        case e: JsError =>
          logger.error("Failed parsing settings " +
            e.recoverTotal(e => JsError.toJson(e)))
          sys.exit(1)
      }
    }
  }
}

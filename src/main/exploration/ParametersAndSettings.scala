package exploration

import com.typesafe.scalalogging.Logger
import lift.arithmetic.{ArithExpr, Cst}
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.libs.functional.syntax._

object SearchParameters {
  // matrix size
  private val default_size = 1024

  // Minimum number of work item per workgroup
  private val min_work_items = 128

  // Minimal global grid size
  private val min_grid_size = 4

  // Max amount of private memory allocated (this is not necessarily the number of registers)
  private val max_amount_private_memory = 1024

  // Max static amount of local memory
  private val max_amount_local_memory = 49152

  // Minimum number of workgroups
  private val min_num_workgroups = 8

  // Maximum number of workgroups
  private val max_num_workgroups = 10000

  def createDefault = createWithDefaults(None, None, None, None, None, None, None)

  def createWithDefaults(
    defaultSize: Option[Int],
    minWorkItems: Option[Int],
    minGridSize: Option[Int],
    maxPrivateMemory: Option[Int],
    maxLocalMemory: Option[Int],
    minWorkgroups: Option[Int],
    maxWorkgroups: Option[Int]
  ) = SearchParameters(
    defaultSize.getOrElse(default_size),
    minWorkItems.getOrElse(min_work_items),
    minGridSize.getOrElse(min_grid_size),
    maxPrivateMemory.getOrElse(max_amount_private_memory),
    maxLocalMemory.getOrElse(max_amount_local_memory),
    minWorkgroups.getOrElse(min_num_workgroups),
    maxWorkgroups.getOrElse(max_num_workgroups)
  )

}

case class SearchParameters(
  defaultSize: Int,
  minWorkItems: Int,
  minGridSize: Int,
  maxPrivateMemory: Int,
  maxLocalMemory: Int,
  minWorkgroups: Int,
  maxWorkgroups: Int
)

case class Settings(
  inputCombinations: Option[Seq[Seq[ArithExpr]]] = None,
  searchParameters: SearchParameters = SearchParameters.createDefault
) {

  override def toString: String = {
    s"""Settings(
       |  $inputCombinations,
       |  $searchParameters
       |)""".stripMargin
  }

}

object ParseSettings {

  private val logger = Logger(this.getClass)

  private[exploration] implicit val arithExprReads: Reads[ArithExpr] =
    JsPath.read[Long].map(Cst)

  private[exploration] implicit val parametersReads: Reads[SearchParameters] = (
    (JsPath \ "default_size").readNullable[Int] and
    (JsPath \ "min_work_items").readNullable[Int] and
    (JsPath \ "min_grid_size").readNullable[Int] and
    (JsPath \ "max_private_memory").readNullable[Int] and
    (JsPath \ "max_local_memory").readNullable[Int] and
    (JsPath \ "min_workgroups").readNullable[Int] and
    (JsPath \ "max_workgroups").readNullable[Int]
  )(SearchParameters.createWithDefaults _)

  private[exploration] implicit val settingsReads: Reads[Settings] = (
    (JsPath \ "input_combinations").readNullable[Seq[Seq[ArithExpr]]] and
    (JsPath \ "search_parameters").readNullable[SearchParameters]
  )((maybeCombinations, maybeParameters) =>
    Settings(
      maybeCombinations,
      maybeParameters.getOrElse(SearchParameters.createDefault)
    ))

  def apply(optionFilename: Option[String]): Settings =
    optionFilename match {
      case Some(filename) =>

        val settingsString = ParameterRewrite.readFromFile(filename)
        val json = Json.parse(settingsString)
        val validated = json.validate[Settings]

        validated match {
          case JsSuccess(settings, _) =>
            checkSettings(settings)
            settings
          case e: JsError =>
            logger.error("Failed parsing settings " +
              e.recoverTotal( e => JsError.toFlatJson(e) ))
            sys.exit(1)
        }

      case None => Settings()
    }

  private def checkSettings(settings: Settings): Unit = {

    // Check all combinations are the same size
    if (settings.inputCombinations.isDefined) {
      val sizes = settings.inputCombinations.get

      if (sizes.map(_.length).distinct.length != 1) {
        logger.error("Sizes read from settings contain different numbers of parameters")
        sys.exit(1)
      }
    }
  }

}

package exploration

import ParseSettings._
import lift.arithmetic.ArithExpr
import org.junit.Test
import org.junit.Assert._
import play.api.libs.json._

class TestParametersAndSettings {

  @Test
  def generateCompleteDefaultConfigString(): Unit = {
    val config = Settings.generateConfigString
    println(config)
    val json = Json.parse(config)
    val validated = json.validate[Settings]
    validated match {
      case JsSuccess(settings, _) =>
        assertEquals(Settings(), settings)
      case _: JsError => fail()
    }
  }

  @Test
  def checkValidConfigGeneration(): Unit = {
    val config = HighLevelRewriteSettings.generateConfigString
    val json = Json.parse(config)
    val validated = json.validate[Settings]
    validated match {
      case JsSuccess(settings, _) =>
        assertEquals(Settings(), settings)
      case _: JsError => fail()
    }
  }

  @Test
  def checkSingleValue(): Unit = {
    val config = s"""
      |{
      |  "${HighLevelRewriteSettings.keyHighLevelRewrite}" : {
      |    "${HighLevelRewriteSettings.keyExplorationDepth}" : ${HighLevelRewriteSettings.defaultExplorationDepth}
      |  }
      |}
    """.stripMargin
    val json = Json.parse(config)
    val validated = json.validate[Settings]
    validated match {
      case JsSuccess(settings, _) =>
        assertEquals(Settings(), settings)
      case _: JsError => fail()
    }
  }

  @Test
  def checkParameterConstraint(): Unit = {
    // value cannot be negative
    val config = s"""
      |{
      |  "${HighLevelRewriteSettings.keyHighLevelRewrite}" : {
      |    "${HighLevelRewriteSettings.keyExplorationDepth}" : -1
      |  }
      |}
    """.stripMargin
    val json = Json.parse(config)
    val validated = json.validate[HighLevelRewriteSettings]
    validated match {
      case JsSuccess(settings, _) =>
        fail()
      case _: JsError => assertTrue(true)
    }
  }

  @Test
  def checkSetSingleValue(): Unit = {
    val value = 42
    val config = s"""
      |{
      |  "${HighLevelRewriteSettings.keyHighLevelRewrite}" : {
      |    "${HighLevelRewriteSettings.keyExplorationDepth}" : $value
      |  }
      |}
    """.stripMargin
    val json = Json.parse(config)
    val validated = json.validate[Settings]
    validated match {
      case JsSuccess(settings, _) =>
        assertEquals(value, settings.highLevelRewriteSettings.explorationDepth)
      case _: JsError => fail()
    }
  }

  @Test
  def typoDetection(): Unit = {
    val config = s"""
      |{
      |  "${HighLevelRewriteSettings.keyHighLevelRewrite}" : {
      |    "exploraton_depth" : 42
      |  }
      |}
    """.stripMargin
    val json = Json.parse(config)
    val validated = json.validate[Settings]
    validated match {
      case JsSuccess(settings, _) =>
        fail()
      case _: JsError => assertTrue(true)
    }
  }

  @Test
  def emptySettings(): Unit = {

    val string =
      """
        |{
        |}
      """.stripMargin

    val json = Json.parse(string)
    val validated = json.validate[Settings]

    validated match {
      case JsSuccess(settings, _) =>
        assertEquals(Settings(), settings)
      case _: JsError => fail()
    }
  }

  @Test
  def onlyInputSettings(): Unit = {

    val string =
      """
        |{
        |  "input_combinations" : [
        |    [1,2,3],
        |    [2,2,2]
        |  ]
        |}
      """.stripMargin

    val json = Json.parse(string)
    val validated = json.validate[Settings]

    validated match {
      case JsSuccess(settings, _) =>
        val settingsGold =
          Settings(Some(Seq(
            Seq[ArithExpr](1, 2, 3),
            Seq[ArithExpr](2,2,2)
          )))
        assertEquals(settingsGold, settings)
      case _: JsError => fail()
    }
  }

  @Test
  def emptySearchSettings(): Unit = {

    val string =
      """
        |{
        |  "search_parameters" : {
        |  }
        |}
      """.stripMargin

    val json = Json.parse(string)
    val validated = json.validate[Settings]

    validated match {
      case JsSuccess(settings, _) =>
        assertEquals(Settings(), settings)
      case _: JsError => fail()
    }
  }

  @Test
  def onlySearchSettings(): Unit = {

    val string =
      """
        |{
        |  "search_parameters" : {
        |    "min_global_size" : 1024,
        |    "default_input_size" : 512
        |  }
        |}
      """.stripMargin

    val json = Json.parse(string)
    val validated = json.validate[Settings]

    validated match {
      case JsSuccess(settings, _) =>
        assertEquals(1024, settings.searchParameters.minGlobalSize)
        assertEquals(512, settings.searchParameters.inputSize)
      case _: JsError => fail()
    }
  }

  @Test
  def bothSettings(): Unit = {

    val string =
      """
        |{
        |  "input_combinations" : [
        |    [1024, 512],
        |    [512, 1024]
        |  ],
        |  "search_parameters" : {
        |    "max_workgroups" : 1024,
        |    "max_local_memory" : 32768
        |  }
        |}
      """.stripMargin

    val json = Json.parse(string)
    val validated = json.validate[Settings]

    val sizesGold = Some(Seq(
      Seq[ArithExpr](1024, 512),
      Seq[ArithExpr](512, 1024)
    ))

    validated match {
      case JsSuccess(settings, _) =>
        assertEquals(sizesGold, settings.inputCombinations)
        assertEquals(1024, settings.searchParameters.maxWorkgroups)
        assertEquals(32768, settings.searchParameters.maxLocalMemory)
      case _: JsError => fail()
    }
  }

}

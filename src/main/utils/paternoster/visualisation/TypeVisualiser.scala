package utils.paternoster.visualisation

import java.io.{File, PrintWriter}
import javafx.application.Application
import javafx.embed.swing.SwingFXUtils
import javafx.scene.image.WritableImage
import javax.imageio.ImageIO

import ir._
import lift.arithmetic.{SimplifiedExpr, _}
import utils.paternoster.gui.{MainPane, VisualiserWindow}
import utils.paternoster.rendering.Graphics
import utils.paternoster.rendering.Graphics._
import utils.paternoster.visualisation

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TypeVisualiser(argTypes: List[Type], expressionSource: String = "") {

  case class TypeVisualisation(id: Int, argType: Type, dimensionGrouping: List[List[ArithExpr]])

  val INITIAL_VAR_VALUE = 4;


  var variables: mutable.HashMap[String, Int] = null
  var typeVisualizations: List[TypeVisualisation] = null
  var expressionSourceCode: String = expressionSource



  /**
    * Initializes the variable list and the list of TypeVisualisations
    */
  def init(): Unit = {
    initVarList()
    initTypeVisualisationList()
  }

  /**
    * Initializes the variable lists.
    */
  def initVarList(): Unit = {
    variables = new mutable.HashMap[String, Int]
    argTypes.collect { case t: Type => t.varList }.flatten.distinct.sortWith(_.name > _.name).foreach(variable => variables.put(variable.name, INITIAL_VAR_VALUE))
  }

  /**
    * Initialises the list of TypeVisualisations
    */
  def initTypeVisualisationList(): Unit = {
      var listBuffer = new ListBuffer[TypeVisualisation]
      var index = 0;
      argTypes.foreach(argType => {
        listBuffer += TypeVisualisation(index, argType, getDimensionGrouping(argType))
        index += 1
      })
      typeVisualizations = listBuffer.toList
  }

  /**
    * Starts the GUI
    *
    * @param args there are no parameters.
    */
  def startGui(args: Array[String] = Array("default argument")) = {
    Application.launch(classOf[VisualiserWindow], args: _*)
  }

  /**
    * Getter for the list of TypeVisualisations.
    *
    * @return the list of TypeVisualisations.
    */
  def getTypeVisualizations(): List[TypeVisualisation] = {
    typeVisualizations
  }

  /**
    * Getter for the variable names.
    *
    * @return A list of all variable names.
    */
  def getVariableNames(): List[String] = {
    variables.keySet.toList
  }

  /**
    * Getter for all variables and variable values.
    *
    * @return The mapping of variable name and value.
    */
  def getVariables(): mutable.HashMap[String, Int] = {
    variables
  }


  /**
    * Setter for a variable.
    *
    * @param varName The name of the variable that the value will be assigned to.
    * @param value   The new value of the variable.
    */
  def setVarValue(varName: String, value: Int): Unit = {
    variables.update(varName, value)
  }

  /**
    * Helper function that counts the number of nested arrays in the given type.
    *
    * @param argType The type that will be searched for arrays.
    * @return The number of arrays that are nested in the given type.
    */
  def getDimensionCount(argType: Type): Int = {
    argType match {
      case ar: ArrayType with Size => 1 + getDimensionCount(ar.elemT)
      case vt: VectorType => 1
      case other => 0
    }
  }

  /**
    * The function generates the default Dimension-Grouping for the given type.
    *
    * @param argType The type for wich the Dimension-Grouping will be generated.
    * @return The default Dimension-Grouping of the given type.
    */
  def getDimensionGrouping(argType: Type): List[List[ArithExpr]] = {
    argType match {
      case ar: ArrayType with Size => List(ar.size) :: getDimensionGrouping(ar.elemT)
      case vt: VectorType => List(vt.len) :: List()
      case tt: TupleType =>{
        var buffer = new ListBuffer[List[List[ArithExpr]]]()
        for( tupleElement <- tt.elemsT){
          buffer +=  getDimensionGrouping(tupleElement)
        }
        buffer.toList.flatten
      }
      case other => List()
    }
  }

  /**
    * Renders everything and draws all visualisations in the GUI.
    *
    * @param drawPane The MainPane that the visualisation will be drawn to.
    * @throws Exception If the array-dimension is unsupported.
    */
  @throws(classOf[Exception])
  @throws(classOf[RuntimeException])
  def draw(drawPane: MainPane): Unit = {
      drawPane.draw(renderNodes())
  }

  /**
    * Returns the String representation of a type. But variable names like "v_N_3" will be
    * replaced with the normal variable name "N".
    *
    * @param argType The type that will be converted to a String representation.
    * @return The modified name of the given type.
    */
  def getCleanedTypeName(argType: Type): String = {
    var typeString = argType.toString
    getVariableNames().foreach(varName => {
      var pattern = "v_" + varName + "_\\d"
      typeString = typeString.replaceAll(pattern, varName)
    })
    typeString
  }

  /**
    * Validates the grouping String that the user has typed.
    *
    * @param referencedVisualisation The visualisation for wich the grouping will be changed.
    * @param grouping                The grouping String that the user typed.
    * @return "true" if the grouping was valid or "false" if it was not.
    */
  def checkGroupingValid(referencedVisualisation: TypeVisualisation, grouping: String): Boolean = {

    var defaultGroupingString = getDimensionGroupingAsString(referencedVisualisation.argType)
    var pattern = replaceArraysWithPatterns(defaultGroupingString)
    pattern = "(" + escapeSpecialRegexCharacters(pattern) + ")"
    var matches = grouping.matches(pattern)
    matches
  }

  /**
    * Helper function that escapes all clauses in the regex.
    *
    * @param pattern The pattern in wich the clauses will be escaped.
    * @return The pattern with escaped clauses.
    */
  def escapeSpecialRegexCharacters(pattern: String): String = {
       var result = pattern.replaceAll("\\[", "\\\\[")
    result = result.replaceAll("\\]", "\\\\]")
    result
  }

  /**
    * Recursive method that replaces the arrays in the standard Dimension-Grouping String
    * with a regex of all possible groupings for the arrays.
    *
    * @param grouping The grouping String.
    * @param index    Indicates the current nesting depth of an array.
    * @return The modified grouping String.
    */
  def replaceArraysWithPatterns(grouping: String, index: Int = 0): String = {
    val openingClause = grouping.indexOf("(", index)
    if (openingClause.equals(-1)) {
      return grouping.substring(index)
    }
    var closingClause = grouping.indexOf(")", index)

    while (closingClause + 1 < grouping.size && grouping.charAt(closingClause + 1) == '(') {
      //In this case we have a multidminensional array
      closingClause = grouping.indexOf(")", closingClause + 1)
    }
    var section = grouping.substring(openingClause, closingClause + 1)
    var arrayCount = section.count(_ == '(')
    var replacement = getArrayGroupingCombinationPattern(arrayCount)
    var returnString = grouping.subSequence(index, openingClause).toString
    returnString += "("+escapeSpecialRegexCharacters(replacement)+")"
    returnString += replaceArraysWithPatterns(grouping, closingClause + 1)

    return returnString
  }

  /**
    * Generates a regex that matches all possible groupings of an array with
    * the nesting depth arrayCount.
    *
    * @param arrayCount The nesting depth of the array.
    * @return regex that matches all possible groupings of an array with the given depth.
    */
  def getArrayGroupingCombinationPattern(arrayCount: Int): String = {
    var pattern = ""
    //1 represents not grouped 2 represents grouped
    var allCodedGroupings = getAllPossibleGroupings(arrayCount)
    allCodedGroupings.foreach(codedGrouping => pattern += getSingleGroupingAsPattern(codedGrouping) + "|")
    pattern.substring(0, pattern.length - 1)
  }

  /**
    * Helper method that generates a regex based on the grouping coded as 1s and 2s.
    *
    * @param grouping The coded grouping. One stands for not grouped and two stands for grouped.
    * @return The regex that matches the given grouping.
    */
  def getSingleGroupingAsPattern(grouping: List[Int]): String = {
    var pattern = ""
    grouping.foreach(number => {
      if (number.equals(1)) {
        pattern += "\\(\\d+\\)"
      } else {
        pattern += "\\(\\d+,\\d+\\)"
      }
    })
    pattern
  }

  /**
    * Generates a coding for all possible groupings of arrayCount Arrays.
    *
    * @param arrayCount The number of arrays for wich all groupings will be found.
    * @return A list containing all possible groupings. One stands for not grouped and two stands for grouped.
    */
  def getAllPossibleGroupings(arrayCount: Int): List[List[Int]] = {
    var codedStandartGrouping = List.fill(arrayCount)(1)
    var uniqueGroupingBuffer = new ListBuffer[List[Int]]()
    uniqueGroupingBuffer += codedStandartGrouping
    getAllUniqueGroupings(codedStandartGrouping, uniqueGroupingBuffer)
    var allUniqueGroupings = uniqueGroupingBuffer.toList
    var allPossibleGroupingsBuffer = new ListBuffer[List[Int]]()
    allUniqueGroupings.foreach(uniqueGrouping => uniqueGrouping.permutations.toList.foreach(permutation => allPossibleGroupingsBuffer += permutation))
    return allPossibleGroupingsBuffer.toList
  }

  /**
    * Helper method that generates all unique groupings. Unique means that {1,2,1} = {1,1,2}.
    * The results are saved in allGroupings.
    *
    * @param currentGrouping The grouping from wich the new grouping will be generated.
    * @param allGroupings    The List of all found groupings.
    */
  def getAllUniqueGroupings(currentGrouping: List[Int], allGroupings: ListBuffer[List[Int]]): Unit = {
    if (currentGrouping.size >= 2) {
      var firstElem = currentGrouping.take(1).head
      var secondElem = currentGrouping.take(2).tail.head
      if (firstElem == 1 && secondElem == 1) {
        var buffer = new ListBuffer[Int]()
        currentGrouping.drop(2).foreach(number => buffer += number)
        buffer += 2
        var newGrouping = buffer.toList
        allGroupings += newGrouping
        getAllUniqueGroupings(newGrouping, allGroupings)
      }
    }
  }


  /**
    * Processing of the user input for the Dimension-Grouping. The input will be validated. If it is
    * valid then the Dimension-Grouping will be updated.
    *
    * @param id       The id of the TypeVisualisation for wich the grouping will be changed.
    * @param grouping The user input for the Dimension-Grouping.
    */
  def updateDimensionGrouping(id: String, grouping: String): Unit = {
    var referencedVisualisation = typeVisualizations.filter(tv => tv.id.toString.equals(id)).head
    if (checkGroupingValid(referencedVisualisation: TypeVisualisation, grouping: String)) {
      var defaultGrouping = getDimensionGrouping(referencedVisualisation.argType)
      //var flatSizes = currentGrouping.flatten.reverse
      var flatSizes = defaultGrouping.flatten
      var parsedGrouping = parseDimensionGrouping(grouping)
      var newGrouping = parsedGrouping.map(group => {
        var sizes = flatSizes.take(group.size)
        flatSizes = flatSizes.drop(group.size)
        sizes
      })
      typeVisualizations = typeVisualizations.updated(typeVisualizations.indexOf(referencedVisualisation), referencedVisualisation.copy(referencedVisualisation.id, referencedVisualisation.argType, newGrouping))
    } else {
      throw new IllegalArgumentException("The dimension grouping: \"" + grouping + "\" is invalid.")
    }

  }

  /**
    * Returns the standard Dimension-Grouping for the given type as String.
    *
    * @param argType    The type for wich the String will be returned.
    * @param arrayDepth Recursion variable that counts the number of consecutive arrays.
    * @return The String representation of the standard Dimension-Grouping.
    */
  def getDimensionGroupingAsString(argType: Type, arrayDepth: Int = 1): String = {
    //only for default groupings...
    argType match {
      case t: TupleType => {
        var stringRepresentation = "[ "
        for (elem <- t.elemsT) {
          stringRepresentation += getDimensionGroupingAsString(elem) + " , "
        }
        //remove the last " , " and add closing clause
        stringRepresentation = stringRepresentation.substring(0, stringRepresentation.length - 3) + " ]"
        stringRepresentation
      }
      case a: ArrayType with Size => {
        var nestedArrays = a.elemT match {
          case nested: ArrayType with Size => getDimensionGroupingAsString(nested, arrayDepth + 1)
          case tuple: TupleType => getDimensionGroupingAsString(tuple)
          case default => ""
        }
        "(" + arrayDepth + ")" + nestedArrays
      }
      case v: VectorType => "V"
      case s: ScalarType => "S"
      case _ => throw new NotImplementedError()
    }
  }

  /**
    * Processing of the user input for a variable value. It will be checked if the
    * varable value leads to invalid array sizes. If that is the case an exception will be thrown.
    *
    * @param varName  The name of the variable that will be updated.
    * @param newValue The new value of the variable.
    * @throws java.lang.NumberFormatException If the given value is not an integer.
    * @throws ir.TypeException                If the given value leads to invalid array sizes.
    */
  @throws(classOf[NumberFormatException])
  @throws(classOf[TypeException])
  def updateVariable(varName: String, newValue: String): Unit = {
    val oldVal = variables.get(varName).get
    variables.update(varName, Integer.parseInt(newValue))
    try {
      typeVisualizations.foreach(tv => {
        getGroupingWithValues(tv.dimensionGrouping)
      })
    } catch {
      case tex: TypeException => {
        //revert change and throw exception
        variables.update(varName, oldVal)
        throw tex
      }
    }
  }

  /**
    * Parsing of the Dimension-Grouping that the user has typed in.
    *
    * @param grouping The String representation of the grouping.
    * @throws java.lang.NumberFormatException If numbers in the grouping can't be read.
    * @return The parsed grouping.
    */
  @throws(classOf[NumberFormatException])
  def parseDimensionGrouping(grouping: String): List[List[Int]] = {
    var listBuffer = new ListBuffer[List[Int]]
    var groupingString = grouping
    while (groupingString.size >= 3) {
      var opening = groupingString.indexOf("(")
      var closing = groupingString.indexOf(")")
      if (!opening.equals(-1) && !closing.equals(-1)) {
        var dimension = groupingString.substring(opening + 1, closing)
        if (dimension.contains(",")) {
          var dimensionSplit = dimension.split(",")
          var dimesions = dimensionSplit.map(str => Integer.parseInt(str))
          listBuffer += dimesions.toList
          groupingString = groupingString.substring(closing + 1)
        } else {
          listBuffer += List(Integer.parseInt(dimension))
          groupingString = groupingString.substring(closing + 1)
        }
      } else {
        return listBuffer.toList
      }
    }
    listBuffer.toList
  }


  /**
    * Function that replaces all variables in the ArithExpr with the saved variable values.
    *
    * @param ae The ArithExrp in wich the variables will be replaced with values.
    * @return The new ArithExpr.
    */
  def insertVarValues(ae: ArithExpr): ArithExpr = {
    var values = getVariables()
    val arithExpVisiterFunc = (ae: ArithExpr) => {
      ae match {
        case v: Var if (values.contains(v.name)) => ArithExpr.longToCst(Int.int2long(values.get(v.name).get))
        case an: Any => an
      }
    }
    ae.visitAndRebuild(arithExpVisiterFunc)
  }

  /**
    * Returns the Dimension-Grouping but evaluates all arithmetic Expressions first.
    *
    * @param grouping The Dimension-Grouping wich will be processed.
    * @throws ir.TypeException If the combination of variable values leads to invalid array sizes.
    * @return The Dimension-Grouping with evaluated expressions.
    */
  @throws(classOf[TypeException])
  def getGroupingWithValues(grouping: List[List[ArithExpr]]): List[List[Int]] = {
    grouping.map(sizes => sizes.map(ae => {
      //ArithExpr(141) check if isWhole is pointless because result is floored in evalDouble
      val evalResult = insertVarValues(ae).evalDouble
      if (evalResult.isWhole()) {
        evalResult.toInt
      } else {
        throw new TypeException("With the current variables the evaluation of:\n"+ae.toString() +"\nyields\n\""+ evalResult.toString + "\" wich is not a valid array size.")
      }
    })).asInstanceOf[List[List[Int]]]
  }

  /**
    * Processing of the save command from the user interface.
    *
    * @param file      The file to wich the user wants to save.
    * @param extension The file format that the user chose.
    */
  def saveGraphic(file: File, extension: String): Unit = {
    extension match {
      case ".svg" => saveAsSvg(file)
      case ".png" => saveAsPng(file)
      case default => throw new NotImplementedError("The export is only possible as .svg or .png file.")
    }
  }

  /**
    * Renders the current visualisation to an svg file and saves it.
    *
    * @param file The file to wich will be saved.
    */
  def saveAsSvg(file: File): Unit = {
    val mainPane = TypeVisualiser.getMainPane()
    var svgString = mainPane.renderToSvg(renderNodes(),getVisualisationDimension())

    val out = new PrintWriter(file)
    try
      out.println(svgString)
    finally if (out != null) out.close()
  }

  /**
    * Takes a snapshot of the canvas and saves the visualisation as png file.
    *
    * @param file The file to wich will be saved.
    */
  def saveAsPng(file: File): Unit = {
    val mainPane = TypeVisualiser.getMainPane()
    var wim = new WritableImage(mainPane.canvas.getWidth.toInt, mainPane.canvas.getHeight.toInt)
    mainPane.getSnapShot(wim)
    ImageIO.write(SwingFXUtils.fromFXImage(wim, null), "png", file)
  }

  /**
    * Helper method that searches the positions of the printType commands in the source code.
    *
    * @return The list of begin and end indicies of the commands.
    */
  def getPrintTypeIndicies(): List[(Int, Int)] = {
    var pattern = "PrintType("
    var fromIndex = 0
    var listBuffer = new ListBuffer[(Int, Int)]
    while (fromIndex < expressionSourceCode.length) {
      var beginIndex = expressionSourceCode.indexOf(pattern, fromIndex)
      if (beginIndex == -1) return listBuffer.toList
      var endIndex = expressionSourceCode.indexOf(')', beginIndex)
      val indicies = (beginIndex, endIndex)
      listBuffer += indicies
      fromIndex = endIndex + 1
    }
    return listBuffer.toList
  }

  def getVisualisationDimension(): (Double,Double) ={
    try {
      var nodeBuffer = new ListBuffer[Iterable[Graphics.GraphicalPrimitive]]

      var firstType = true;
      var yMargin = 1f;
      var yMarginBetweenTypes = 5
      var accHeight = 5d;
      var maxWidth = 0.0d
      var expressionBuffer = new ListBuffer[Graphics.ExpressionSource]

      if (expressionSourceCode != "" && typeVisualizations.length > 0) {
        var printTypeIndicies = getPrintTypeIndicies()
        for (indicies <- printTypeIndicies) {
          expressionBuffer += Graphics.ExpressionSource(expressionSourceCode, indicies._1, indicies._2, 0, 0)
        }
        if (printTypeIndicies.length != typeVisualizations.length) {
          throw new IllegalArgumentException("The number of PrintType calls in the source code must match the number of displayed types.")
        }
      }
      var expressionTexts = expressionBuffer.toList.reverse


      for (tv <- typeVisualizations) {
        val currentGrouping = tv.dimensionGrouping
        var groupingWithValues = getGroupingWithValues(currentGrouping)
        nodeBuffer += Scene.drawType(visualisation.Scene.typeNode(tv.argType, groupingWithValues))
      }


      var allAdjustedNodes = for (nodes <- nodeBuffer.toList) yield {

        var firstBox = nodes.maxBy(gp => gp match {
          case BoxWithText(text, bx, by, bwidth, bheight) => {
            if(bwidth>maxWidth) maxWidth = bwidth
            bwidth
          }
          case Rectangle(x, y, w, h) => {
            if(w>maxWidth) maxWidth = w
            w
          }
          case Box(x, y, w, h) => {
            if(w>maxWidth) maxWidth = w
            w
          }
          case CorneredClause(x, y, w, h) => {
            if(w>maxWidth) maxWidth = w
            w
          }
          case Seperator(x, y) => 0
          case ExpressionSource(_, _, _, _, _) => 0
          case DashedBox(_, _, w, _) => {
            if(w>maxWidth) maxWidth = w
            w
          }
          case _ => 0
        })


        var adjustedNodes: Iterable[GraphicalPrimitive] = null

        if (expressionTexts.size > 0) {
          var currentExpressionText: Graphics.GraphicalPrimitive = expressionTexts.take(1).head;
          expressionTexts = expressionTexts.drop(1)

          var expressionText = currentExpressionText match {
            case ExpressionSource(text, _, _, _, _) => text
            case _ => throw new NotImplementedError()
          }
          val mainPane = TypeVisualiser.getMainPane()
          var textHeight = mainPane.getStringHeight(expressionText, mainPane.getExpressionFontFx()).ceil + 1

          currentExpressionText = Graphics.translate(currentExpressionText, 0, accHeight - 1)

          if(expressionText.contains("\n")){
            var lineHeight = mainPane.getStringHeight("T", mainPane.getExpressionFontFx()).ceil
            accHeight+=lineHeight*2
          }
          accHeight += textHeight


          adjustedNodes = Graphics.translateAll(nodes, 0, accHeight)

          adjustedNodes = Seq(currentExpressionText) ++ adjustedNodes
        } else {
          adjustedNodes = Graphics.translateAll(nodes, 0, accHeight)
        }


        //add the current height to the accumulatedHeight variable.
        firstBox match {
          case BoxWithText(text, bx, by, bwidth, bheight) => accHeight += bheight + yMargin
          case Rectangle(x, y, w, h) => accHeight += h
          case Box(x, y, w, h) => accHeight += h
          case CorneredClause(x, y, w, h) => accHeight += h
          case DashedBox(_, _, w, _) => accHeight += w
          case _ => throw new NotImplementedError()
        }
        firstType = false
        accHeight += yMarginBetweenTypes
        adjustedNodes
      }

      return (maxWidth,accHeight)
    }catch{
      case e : Exception => throw e
    }
  }

  /**
    * Creates all GraphicalPrimitives to draw the visualisation. It renders the nodes first and then it moves
    * the different datatypes so that they will be displayed under each other.
    *
    * @return The primitives of the visualisation.
    * @throws Exception If the rendering encounters unsupported array-dimensions.
    */
  @throws(classOf[Exception])
  def renderNodes(): Iterable[Graphics.GraphicalPrimitive] = {
    try {
      var nodeBuffer = new ListBuffer[Iterable[Graphics.GraphicalPrimitive]]

      var firstType = true;
      var yMargin = 1f;
      var yMarginBetweenTypes = 5
      var accHeight = 5d;
      var expressionBuffer = new ListBuffer[Graphics.ExpressionSource]

      if (expressionSourceCode != "" && typeVisualizations.length > 0) {
        var printTypeIndicies = getPrintTypeIndicies()
        for (indicies <- printTypeIndicies) {
          expressionBuffer += Graphics.ExpressionSource(expressionSourceCode, indicies._1, indicies._2, 0, 0)
        }
        if (printTypeIndicies.length != typeVisualizations.length) {
          throw new IllegalArgumentException("The number of PrintType calls in the source code must match the number of displayed types.")
        }
      }
      var expressionTexts = expressionBuffer.toList.reverse


      for (tv <- typeVisualizations) {
        val currentGrouping = tv.dimensionGrouping
        var groupingWithValues = getGroupingWithValues(currentGrouping)
        nodeBuffer += Scene.drawType(visualisation.Scene.typeNode(tv.argType, groupingWithValues))
      }


      var allAdjustedNodes = for (nodes <- nodeBuffer.toList) yield {

        var firstBox = nodes.maxBy(gp => gp match {
          case BoxWithText(text, bx, by, bwidth, bheight) => bwidth
          case Rectangle(x, y, w, h) => w
          case Box(x, y, w, h) => w
          case CorneredClause(x, y, w, h) => w
          case Seperator(x, y) => 0
          case ExpressionSource(_, _, _, _, _) => 0
          case DashedBox(_, _, w, _) => w
          case _ => 0
        })

        var adjustedNodes: Iterable[GraphicalPrimitive] = null

        if (expressionTexts.size > 0) {
          var currentExpressionText: Graphics.GraphicalPrimitive = expressionTexts.take(1).head;
          expressionTexts = expressionTexts.drop(1)

          var expressionText = currentExpressionText match {
            case ExpressionSource(text, _, _, _, _) => text
            case _ => throw new NotImplementedError()
          }
          val mainPane = TypeVisualiser.getMainPane()
          var textHeight = mainPane.getStringHeight(expressionText, mainPane.getExpressionFontFx()).ceil + 1

          currentExpressionText = Graphics.translate(currentExpressionText, 0, accHeight - 1)

          if(expressionText.contains("\n")){
            var lineHeight = mainPane.getStringHeight("T", mainPane.getExpressionFontFx()).ceil
            accHeight+=lineHeight*2
          }
          accHeight += textHeight


          adjustedNodes = Graphics.translateAll(nodes, 0, accHeight)

          adjustedNodes = Seq(currentExpressionText) ++ adjustedNodes
        } else {
          adjustedNodes = Graphics.translateAll(nodes, 0, accHeight)
        }


        //add the current height to the accumulatedHeight variable.
        firstBox match {
          case BoxWithText(text, bx, by, bwidth, bheight) => accHeight += bheight + yMargin
          case Rectangle(x, y, w, h) => accHeight += h
          case Box(x, y, w, h) => accHeight += h
          case CorneredClause(x, y, w, h) => accHeight += h
          case DashedBox(_, _, w, _) => accHeight += w
          case _ => throw new NotImplementedError()
        }
        firstType = false
        accHeight += yMarginBetweenTypes
        adjustedNodes
      }

      allAdjustedNodes.flatten
    }catch{
      case e : Exception => throw e
    }
  }

  init()

}


/**
  * Companion object of the TypeVisualiser.
  */
object TypeVisualiser {
  //Buffers for the visualisation
  var types: ListBuffer[Type] = null;
  var expressionSource = "";

  //references for the communication between gui and visualiser
  var mainPane: MainPane = null;
  var visualiser: TypeVisualiser = null;

  //Lift-Interface to the visualiser
  def apply(argType: Type, render: Boolean = false, expression: String): Unit = {
    if (expression != "") expressionSource = expression

    addType(argType)
    if (render) {
      visualiser = new TypeVisualiser(types.toList, expressionSource)
      visualiser.startGui(Array("default argument"))
    }
  }

  //method to add a type to the visualisation
  def addType(argType: Type): Unit = {
    if (types == null) types = new ListBuffer[Type]()

    types += argType
  }

  def setMainPane(mainPane: MainPane): Unit = {
    this.mainPane = mainPane
  }

  def getMainPane(): MainPane = {
    this.mainPane
  }
}



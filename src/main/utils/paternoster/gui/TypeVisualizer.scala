package utils.paternoster.gui


import java.io.File
import java.util.regex.Pattern
import java.util.stream.Collectors
import javafx.application.Application
import javafx.embed.swing.SwingFXUtils
import javafx.scene.image.WritableImage
import javafx.scene.text.Text
import javax.imageio.ImageIO

import ir._
import lift.arithmetic.ArithExpr.evalDouble
import lift.arithmetic.NotEvaluableException.NotEvaluable
import lift.arithmetic.{SimplifiedExpr, _}
import utils.paternoster.logic.Graphics
import utils.paternoster.logic.Graphics._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TypeVisualizer(argTypes :List[Type],expressionSource :String = "")  {
  case class TypeVisualisation(id : Int, argType:Type, dimensionGrouping: List[List[ArithExpr]])
  val INITIAL_VAR_VALUE = 4;

  var types = argTypes
  var variables: mutable.HashMap[String,Int]=null
  var typeVisualizations : List[TypeVisualisation]= null
  var expressionSourceCode: String = expressionSource


  def init(): Unit ={
    initVarList()
    initTypeVisualisationList()
  }

  def initVarList():Unit ={
    variables= new mutable.HashMap[String,Int]
    types.collect{ case t: Type => t.varList}.flatten.distinct.sortWith(_.name>_.name).foreach(variable=> variables.put(variable.name,INITIAL_VAR_VALUE))
  }

  def initTypeVisualisationList():Unit={
    var listBuffer = new ListBuffer[TypeVisualisation]
    var index = 0;
    types.foreach(argType => {
      listBuffer+= TypeVisualisation(index,argType,getDimensionGrouping(argType))
      index +=1
    })
    typeVisualizations = listBuffer.toList
  }

  def startGui(args:Array[String]= Array("default argument")) = {
    Application.launch(classOf[VisualizerWindow], args:_*)
  }

  def getTypes(): List[Type]={
    types
  }
  def getTypeVisualizations(): List[TypeVisualisation]={
    typeVisualizations
  }

  def getVariableNames(): List[String]={
       variables.keySet.toList
  }
  def getVariables():mutable.HashMap[String,Int]={
     variables
  }
/*  def getDimensionGrouping():mutable.HashMap[Type, List[List[ArithExpr]]] ={
    //Todo Check if ts still necessary to copy this
     var mapClone = new mutable.HashMap[Type, List[List[ArithExpr]]]()
    dimensionGrouping.keySet.foreach(argType => {
      val outerList =  dimensionGrouping.get(argType).get
      val outerListCloneBuffer = new ListBuffer[List[ArithExpr]]
      outerList.foreach(list =>{
        val innerList = list
        val innerListCloneBuffer = new ListBuffer[ArithExpr]
        list.foreach(ae =>{
          var newAe = copy(ae)
          innerListCloneBuffer+= newAe
        })
        outerListCloneBuffer+= innerListCloneBuffer.toList
      })
      mapClone.put(argType,outerListCloneBuffer.toList)
    })
    mapClone
  }
*/
def copy(arithExpr: ArithExpr): ArithExpr= arithExpr match {

  case Cst(c) => Cst(c)
  case IntDiv(n, d) => IntDiv(copy(n),copy(d))
  case Pow(base, exp) =>  Pow(copy(base), copy(exp))

  case Log(b, x) => Log(copy(b), copy(x))

  case Mod(dividend, divisor) => Mod(copy(dividend), copy(divisor))

  case Sum(terms) => Sum(terms.map(term => copy(term)))
  case Prod(terms) => Prod(terms.map(term => copy(term)))

  case FloorFunction(expr) => FloorFunction(copy(expr))
  case CeilingFunction(expr) => CeilingFunction(copy(expr))

  case AbsFunction(expr) => AbsFunction(copy(expr))

  case IfThenElse(test, t, e) => IfThenElse(test, copy(t), copy(e))
  case Var(name,range) => Var(name,range)
  case aeFunc : ArithExprFunction => aeFunc
  case simplEx: SimplifiedExpr => simplEx
  case thing :  lift.arithmetic.?.type => thing
  case negInf : lift.arithmetic.NegInf.type => negInf
  case posInf : lift.arithmetic.PosInf.type => posInf
}


  def setVarValue(varName: String , value: Int): Unit ={
    variables.update(varName,value)
  }

  def getDimensionCount(argType:Type): Int={
    argType match {
    case ar: ArrayType with Size => 1 + getDimensionCount(ar.elemT)
    case vt: VectorType => 1
    case other => 0
  }
  }
  def getDimensionGrouping(argType:Type): List[List[ArithExpr]] ={
    argType match {
      case ar: ArrayType with Size => List(ar.size) :: getDimensionGrouping(ar.elemT)
      case vt: VectorType => List(vt.len) :: List()
      case other => List()
    }
  }

  def draw(drawPane: MainPane):Unit ={
    drawPane.draw(renderNodes())
  }

  def getCleanedTypeName(argType:Type): String ={
      var typeString = argType.toString
      getVariableNames().foreach(varName => {
        var pattern = "v_"+varName+"_\\d"
        typeString = typeString.replaceAll(pattern,varName)
      })
      typeString
  }

  def checkGroupingValid(referencedVisualisation: TypeVisualisation , grouping : String ) : Boolean={

    var defaultGroupingString = getDimensionGroupingAsString(referencedVisualisation.argType)
    var pattern = replaceArraysWithPatterns(defaultGroupingString)
    pattern = "("+escapeSpecialRegexCharacters(pattern)+")"
    var matches = grouping.matches(pattern)
    matches
  }
  def escapeSpecialRegexCharacters(pattern:String) :String ={
    var result = pattern.replaceAll("\\(","\\\\(")
    result =result.replaceAll("\\)","\\\\)")
    result =result.replaceAll("\\[", "\\\\[")
    result =result.replaceAll("\\]", "\\\\]")
    result
  }

  def replaceArraysWithPatterns(grouping: String, index : Int=0): String ={
    val openingClause = grouping.indexOf("(",index)
    if( openingClause.equals(-1)){
      return grouping.substring(index)
    }
    var closingClause = grouping.indexOf(")",index)

    while(closingClause+1 < grouping.size && grouping.charAt(closingClause+1)=='('){
      //In this case we have a multidminensional array
      closingClause = grouping.indexOf(")",closingClause+1)
    }
    var section = grouping.substring(openingClause,closingClause+1)
    var arrayCount = section.count(_ == '(')
    var replacement = getArrayGroupingCombinationPattern(arrayCount)
    var returnString = grouping.subSequence(index,openingClause).toString
    returnString+= replacement
    returnString+= replaceArraysWithPatterns(grouping,closingClause+1)

    return returnString
  }

  def getArrayGroupingCombinationPattern( arrayCount : Int): String ={
    var pattern = ""
    //1 represents not grouped 2 represents grouped
    var allCodedGroupings = getAllPossibleGroupings(arrayCount)
    allCodedGroupings.foreach(codedGrouping => pattern+= getSingleGroupingAsPattern(codedGrouping)+"|")
    pattern.substring(0,pattern.length-1)
  }

  def getSingleGroupingAsPattern(grouping : List[Int]): String ={
    var pattern = ""
    grouping.foreach(number => {
      if(number.equals(1)){
        pattern+= "(\\d+)"
      }else{
        pattern += "(\\d+,\\d+)"
      }
    })
    pattern
  }

  def getAllPossibleGroupings(arrayCount:Int): List[List[Int]] ={
    var codedStandartGrouping = List.fill(arrayCount)(1)
    var uniqueGroupingBuffer = new ListBuffer[List[Int]]()
    uniqueGroupingBuffer+= codedStandartGrouping
    getAllUniqueGroupings(codedStandartGrouping,uniqueGroupingBuffer)
    var allUniqueGroupings =uniqueGroupingBuffer.toList
    var allPossibleGroupingsBuffer = new ListBuffer[List[Int]]()
    allUniqueGroupings.foreach(uniqueGrouping => uniqueGrouping.permutations.toList.foreach(permutation => allPossibleGroupingsBuffer+= permutation))
    return allPossibleGroupingsBuffer.toList
  }

  def getAllUniqueGroupings(currentGrouping: List[Int] , allUniques: ListBuffer[List[Int]]): Unit ={
    if(currentGrouping.size>=2){
      var firstElem = currentGrouping.take(1).head
      var secondElem = currentGrouping.take(2).tail.head
      if(firstElem == 1 && secondElem == 1 ){
        var buffer =  new ListBuffer[Int]()
        currentGrouping.drop(2).foreach( number => buffer+= number)
        buffer+= 2
        var newGrouping  =  buffer.toList
        allUniques+= newGrouping
        getAllUniqueGroupings(newGrouping,allUniques)
      }
    }
  }

  // Todo Throw exception if malformed
  def updateDimensionGrouping( id: String, grouping :String ):Unit = {
    var referencedVisualisation = typeVisualizations.filter(tv => tv.id.toString.equals(id)).head
    if(checkGroupingValid(referencedVisualisation: TypeVisualisation ,grouping:String)){
      var defaultGrouping = getDimensionGrouping(referencedVisualisation.argType)
      //var flatSizes = currentGrouping.flatten.reverse
      var flatSizes = defaultGrouping.flatten
      var parsedGrouping = parseDimensionGrouping(grouping)
      var newGrouping = parsedGrouping.map(group=> {
        var sizes = flatSizes.take(group.size)
        flatSizes = flatSizes.drop(group.size)
        sizes
      })
      typeVisualizations = typeVisualizations.updated(typeVisualizations.indexOf(referencedVisualisation), referencedVisualisation.copy(referencedVisualisation.id,referencedVisualisation.argType,newGrouping))
    }else{
      throw new IllegalArgumentException("The dimension grouping: \""+grouping+"\" is invalid.")
    }

  }

  def getDimensionGroupingAsString(argType:Type, arrayDepth: Int = 1): String ={
    //only for default groupings...
    argType match {
      case t: TupleType => {
        var stringRepresentation = "[ "
        for(elem <- t.elemsT){
          stringRepresentation+=getDimensionGroupingAsString(elem)+" , "
        }
        //remove the last " , " and add closing clause
        stringRepresentation= stringRepresentation.substring(0,stringRepresentation.length-3) + " ]"
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
    }
  }
  def getDimensionAsString(grouping: List[ArithExpr], dimension: Int=1):String={
    grouping.size match {
      case 1 => "("+dimension+")"
      case 2 => "("+dimension+","+dimension+1+")"
    }
  }

  @throws(classOf[NumberFormatException])
  @throws(classOf[TypeException])
  def updateVariable( varName : String , newValue: String): Unit ={
    val oldVal = variables.get(varName).get
    variables.update(varName, Integer.parseInt(newValue))
    try{
      typeVisualizations.foreach(tv =>{
        getGroupingWithValues(tv.dimensionGrouping)
      })
    }catch {
      case tex:TypeException =>{
        //revert change and throw exception
        variables.update(varName, oldVal)
        throw tex
      }
    }
  }

  @throws(classOf[NumberFormatException])
  def parseDimensionGrouping(grouping :String): List[List[Int]] ={
    var listBuffer = new ListBuffer[List[Int]]
    var groupingString = grouping
    while(groupingString.size >= 3){
      var opening = groupingString.indexOf("(")
      var closing = groupingString.indexOf(")")
      if(!opening.equals(-1) && !closing.equals(-1)){
        var dimension = groupingString.substring(opening+1,closing)
        if(dimension.contains(",")){
          var dimensionSplit = dimension.split(",")
          var dimesions = dimensionSplit.map(str => Integer.parseInt(str))
          listBuffer+= dimesions.toList
          groupingString= groupingString.substring(closing+1)
        }else{
          listBuffer+= List(Integer.parseInt(dimension))
          groupingString= groupingString.substring(closing+1)
        }
      }else{
        return listBuffer.toList
      }
    }
    listBuffer.toList
  }



  def insertVarValues(ae: ArithExpr):ArithExpr={
    var values = getVariables()
    val arithExpVisiterFunc = (ae: ArithExpr)=>{
      ae match {
        case v: Var if(values.contains(v.name)) => ArithExpr.LongToCst(Int.int2long(values.get(v.name).get))
        case an:Any=>  an
      }
    }
    ae.visitAndRebuild(arithExpVisiterFunc)
  }

  @throws(classOf[TypeException])
  def getGroupingWithValues(grouping: List[List[ArithExpr]]):List[List[Int]]={
    grouping.map(sizes => sizes.map(ae => {
        //ArithExpr(141) check if isWhole is pointless because result is floored in evalDouble
        val evalResult = insertVarValues(ae).evalDouble
        if(evalResult.isWhole()){
          evalResult.toInt
        }else{
          throw new TypeException(evalResult+" is not a valid array size.")
        }
      } )).asInstanceOf[List[List[Int]]]
  }

  @throws(classOf[NotImplementedError])
  def saveGraphic(file: File, extension: String):Unit ={
    extension match {
      case ".svg" => throw new NotImplementedError("Svg Export is not implemented yet.")
      case ".png" => saveAsPng(file)
      case default => throw new NotImplementedError("The export is only possible as .svg or .png file.")
    }
  }

  def saveAsPng(file:File):Unit ={
    val mainPane = TypeVisualizer.getMainPane()
    var wim = new WritableImage(mainPane.canvas.getWidth.toInt, mainPane.canvas.getHeight.toInt)
    mainPane.getSnapShot(wim)
    ImageIO.write(SwingFXUtils.fromFXImage(wim,null),"png",file)
  }


  def getPrintTypeIndicies(): List[(Int,Int)] ={
    var pattern = "PrintType("
    var fromIndex =0
    var listBuffer = new ListBuffer[(Int,Int)]
    while(fromIndex < expressionSourceCode.length){
      var beginIndex = expressionSourceCode.indexOf(pattern,fromIndex)
      if(beginIndex == -1) return  listBuffer.toList
      var endIndex = expressionSourceCode.indexOf(')',beginIndex)
      val indicies = (beginIndex,endIndex)
      listBuffer+= indicies
      fromIndex= endIndex+1
    }
  return listBuffer.toList
  }

  def renderNodes():Iterable[Graphics.GraphicalPrimitive]={
    var nodeBuffer = new ListBuffer[Iterable[Graphics.GraphicalPrimitive]]

    var firstType = true;
    var yMargin = 1f;
    var yMarginBetweenTypes = 5
    var accHeight = 1d;
    var expressionBuffer = new ListBuffer[Graphics.ExpressionSource]

  if(typeVisualizations.length >0){
     var printTypeIndicies = getPrintTypeIndicies()
    for(indicies <- printTypeIndicies){
      expressionBuffer+= Graphics.ExpressionSource(expressionSourceCode,indicies._1, indicies._2,0,0)
    }
    if(printTypeIndicies.length != typeVisualizations.length){
      throw new IllegalArgumentException("The number of PrintTypes must match the number of types")
    }
  }
    var expressionTexts = expressionBuffer.toList.reverse



    typeVisualizations.map(tv => {
      val currentGrouping = tv.dimensionGrouping
      var groupingWithValues = getGroupingWithValues(currentGrouping)
      nodeBuffer += utils.paternoster.logic.Scene.drawType(utils.paternoster.logic.Scene.typeNode(tv.argType,groupingWithValues))
    })

    var allAdjustedNodes = for(nodes <- nodeBuffer.toList) yield {

      var firstBox = nodes.maxBy(gp => gp match {
        case BoxWithText(text, bx, by, bwidth, bheight) => bwidth
        case Rectangle(x, y, w, h) => w
        case Box(x, y, w, h) => w
        case CorneredClause(x, y, w, h) => w
        case Seperator(x,y) => 0
        case ExpressionSource(_,_,_,_,_) => 0
        case DashedBox(_,_,w,_) => w
      })

     /* var adjustedNodes = firstBox match {
        case bwt: BoxWithText => Graphics.translateAll(nodes, -bwt.x, -bwt.y)
        case r: Rectangle => Graphics.translateAll(nodes, -r.x, -r.y)
        case b: Box => Graphics.translateAll(nodes, -b.x, -b.y)
        case c: CorneredClause => Graphics.translateAll(nodes, -c.x, -c.y)
      }

*/
      var currentExpressionText: Graphics.GraphicalPrimitive = expressionTexts.take(1).head;
      expressionTexts= expressionTexts.drop(1)

      var expressionText = currentExpressionText match {
        case ExpressionSource(text, _, _, _, _) => text
      }
      val mainPane = TypeVisualizer.getMainPane()
      var textHeight = mainPane.getStringHeight(expressionText,mainPane.getExpressionFont()).ceil+1
      accHeight+=textHeight
      currentExpressionText = Graphics.translate(currentExpressionText,0 ,accHeight-1)

      var adjustedNodes = Graphics.translateAll(nodes, 0, accHeight)

      adjustedNodes = Seq(currentExpressionText) ++ adjustedNodes
      firstBox match {
        case BoxWithText(text, bx, by, bwidth, bheight) => accHeight+=bheight+yMargin
        case Rectangle(x, y, w, h) => accHeight+=h
        case Box(x, y, w, h) => accHeight+=h
        case CorneredClause(x, y, w, h) => accHeight+=h
        case DashedBox(_,_,w,_) => accHeight+=w
      }
      firstType=false
      accHeight+= yMarginBetweenTypes
      adjustedNodes
    }
    //allAdjustedNodes = allAdjustedNodes.map(node => Graphics.translateAllToRoundCoords(node))

    allAdjustedNodes.flatten
  }

  init()

}




object TypeVisualizer {
  var types: ListBuffer[Type] = null;
  var mainPane:MainPane = null;
  var visualizer:TypeVisualizer = null;
  var expressionSource = "";

  def apply(argType: Type,render:Boolean = false,expression :String): Unit ={
    if(expression != "") expressionSource = expression

    addType(argType)
    if(render){
      visualizer = new TypeVisualizer(types.toList,expressionSource)
      visualizer.startGui(Array("default argument"))
    }
  }

  def addType(argType: Type): Unit ={
    if(types == null) types= new ListBuffer[Type]()

    types+= argType
  }

  //Gucken ob ich das noch brauche
  def setMainPane(mainPane: MainPane): Unit ={
    this.mainPane = mainPane
  }
  def getMainPane():MainPane={
    this.mainPane
  }
}



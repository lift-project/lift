package utils.paternoster.gui


import javafx.application.Application

import ir.{ArrayType, Capacity, Size, Type}
import lift.arithmetic._
import utils.paternoster.logic.Graphics
import utils.paternoster.logic.Graphics.{Box, BoxWithText, GraphicalPrimitive, Rectangle}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TypeVisualizer(argTypes :List[Type])  {
  val INITIAL_VAR_VALUE = 4;

  var types = argTypes;
  var variables: mutable.HashMap[String,Int]=null;
  var dimensionGrouping : mutable.HashMap[Type,List[List[ArithExpr]]]= null;

  def init(): Unit ={
    initVarList()
    initDimensionGrouping()
  }

  def initVarList():Unit ={
    variables= new mutable.HashMap[String,Int]
    types.collect{ case t: Type => t.varList}.flatten.distinct.sortWith(_.name>_.name).foreach(variable=> variables.put(variable.name,INITIAL_VAR_VALUE))
  }

  def initDimensionGrouping():Unit={
    dimensionGrouping = new mutable.HashMap[Type,List[List[ArithExpr]]];
    types.foreach(argType => dimensionGrouping.put(argType,getDimensionGrouping(argType)))
  }

  def startGui(args:Array[String]= Array("default argument")) = {
    Application.launch(classOf[VisualizerWindow], args:_*)
  }

  def getTypes(): List[Type]={
    types
  }

  def getVariableNames(): List[String]={
       variables.keySet.toList
  }
  def getVariables():mutable.HashMap[String,Int]={
     variables
  }
  def getDimensionGrouping():mutable.HashMap[Type, List[List[ArithExpr]]] ={
     dimensionGrouping
  }


  def setVarValue(varName: String , value: Int): Unit ={
    variables.update(varName,value)
  }

  def getDimensionCount(argType:Type): Int={
    argType match {
    case ar: ArrayType with Size => 1 + getDimensionCount(ar.elemT)
    case other => 0
  }
  }
  def getDimensionGrouping(argType:Type): List[List[ArithExpr]] ={
    argType match {
      case ar: ArrayType with Size => List(ar.size) :: getDimensionGrouping(ar.elemT)
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

  // Todo Throw exception if malformed
  def updateDimensionGrouping( argTypeString: String, grouping :String ):Unit = {
    dimensionGrouping.keySet.foreach( argType => if(argType.toString.equals(argTypeString)) {
      var currentGrouping = dimensionGrouping.get(argType).get
      var flatSizes = currentGrouping.flatten.reverse
      var parsedGrouping = parseDimensionGrouping(grouping)
      var newGrouping = parsedGrouping.map(group=> {
        var sizes = flatSizes.take(group.size)
        flatSizes.drop(group.size)
        sizes
      })
      dimensionGrouping.update(argType,newGrouping)
    })
  }

  def getDimensionGroupingAsString(argType: Type): String ={
    var groupingIterator = dimensionGrouping.get(argType).get.iterator
    var dimensionString = "";
    var dimension = if(getDimensionCount(argType) >0 ) getDimensionCount(argType) else 1
    var i = 1;
    while(i<=dimension){
      var currentGrouping = groupingIterator.next()
      dimensionString+= getDimensionAsString(currentGrouping,i)
      if(currentGrouping.size==2) i+=2 else i+=1
    }
    dimensionString
  }
  def getDimensionAsString(grouping: List[ArithExpr], dimension: Int):String={
    grouping.size match {
      case 1 => "("+dimension+")"
      case 2 => "("+dimension+","+dimension+1+")"
    }
  }

  @throws(classOf[NumberFormatException])
  def updateVariable( varName : String , newValue: String): Unit ={
    variables.update(varName, Integer.parseInt(newValue))
  }

  def parseDimensionGrouping(grouping :String): List[List[Int]] ={
    var listBuffer = new ListBuffer[List[Int]]
    var groupingString = grouping
    while(groupingString.size >= 3){
      var opening = groupingString.indexOf("(")
      var closing = groupingString.indexOf(")")
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

  def getGroupingWithValues(grouping: List[List[ArithExpr]]):List[List[Int]]={
    grouping.map(sizes => sizes.map(ae => insertVarValues(ae).evalInt))
  }



  def renderNodes():Iterable[Graphics.GraphicalPrimitive]={
    var nodeBuffer = new ListBuffer[Iterable[Graphics.GraphicalPrimitive]]

    var firstType = true;
    var yMargin = 1f;
    var accHeight = 0d;

    types.map(argType => {
      var currentGrouping = getGroupingWithValues(dimensionGrouping.get(argType).get)
      nodeBuffer += utils.paternoster.logic.Scene.drawType(utils.paternoster.logic.Scene.typeNode(argType,currentGrouping))
    })

    var allAdjustedNodes = for(nodes <- nodeBuffer.toList) yield {

      var firstBox = nodes.maxBy(gp => gp match {
        case BoxWithText(text, bx, by, bwidth, bheight) => bwidth
        case Rectangle(x, y, w, h) => w
        case Box(x, y, w, h) => w
      })

      var adjustedNodes = firstBox match {
        case bwt: BoxWithText => Graphics.translateAll(nodes, -bwt.x, -bwt.y)
        case r: Rectangle => Graphics.translateAll(nodes, -r.x, -r.y)
        case b: Box => Graphics.translateAll(nodes, -b.x, -b.y)
      }
      if(!firstType) {
        adjustedNodes = firstBox match {
          case bwt: BoxWithText => Graphics.translateAll(nodes, 0, accHeight)
          case r: Rectangle => Graphics.translateAll(nodes, 0, accHeight )
          case b: Box => Graphics.translateAll(nodes, 0, accHeight)
        }
      }
      firstBox match {
        case BoxWithText(text, bx, by, bwidth, bheight) => accHeight+=bheight+yMargin
        case Rectangle(x, y, w, h) => accHeight+=h
        case Box(x, y, w, h) => accHeight+=h
      }
      firstType=false
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

  def apply(argType: Type,render:Boolean): Unit ={
    addType(argType)
    if(render){
      visualizer = new TypeVisualizer(types.toList)
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



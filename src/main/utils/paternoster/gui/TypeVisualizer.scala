package utils.paternoster.gui

import java.io.{File, IOException}
import javafx.application.Application
import javafx.geometry.{Insets, Pos}
import javafx.stage.{FileChooser, Stage}
import javafx.scene.layout.VBox

import ir.{ArrayType, Capacity, Size, Type}
import lift.arithmetic._
import javafx._
import javafx.embed.swing.SwingFXUtils
import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.Node
import javafx.scene.control.Alert.AlertType
import javafx.scene.control.{Alert, Button, Label, TextField}
import javafx.scene.image.WritableImage
import javafx.scene.layout.HBox
import javafx.scene.layout.Pane
import javafx.scene.paint.Color
import javafx.scene.text.Font
import javax.imageio.ImageIO

import utils.paternoster.logic.Graphics
import utils.paternoster.logic.Graphics.{Box, BoxWithText, GraphicalPrimitive, Rectangle}
import utils.paternoster.logic.Scene.flattenArraySizes

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TypeVisualizer() extends Application {

  val defaultVarValue = 4
  val drawingWidth =1280
    val drawingHeight = 720



  override def start(stage: Stage): Unit = {
    //Get the Type
    var argTypes = TypeVisualizer.getTypes()

    System.out.println(argTypes.toString)


    //mainPane is where we will draw
    val mainPane = new MainPane(drawingWidth, drawingHeight)

    TypeVisualizer.setMainPane(mainPane)

    //Vbox - Main layout container
    var main = new VBox();
    main.setSpacing(10);
    main.setPadding(new Insets(8, 8, 8, 8));



    //HBox top, display the expression here
    val top: VBox = new VBox
    top.setSpacing(10);


    //HBox middle, the input boxes for the variables will be added here
    val middle: VBox = new VBox
    middle.setSpacing(10);
    middle.setVisible(false);

    //HBox bottom, the drawin pane will be added here
    val bottom = new VBox
    bottom.setPadding(new Insets(0))
    bottom.setSpacing(8)
    bottom.setAlignment(Pos.CENTER_LEFT)

    //Button to save var values
    val buttonDraw = new Button()
    buttonDraw.setText("Draw")
    buttonDraw.setPrefSize(100, 20)
    //Button to save var values


    //Button to save var values
    val saveVars = new Button()
    saveVars.setText("Set Vars")
    saveVars.setPrefSize(100, 20)

    val buttonDimension = new Button()
    buttonDimension.setText("Set Dimensions")
    buttonDimension.setPrefSize(100, 20)

    //Button to save image
    val buttonSave = new Button()
    buttonSave.setText("Save Image")
    buttonSave.setPrefSize(100, 20)
    //Buttonlogic
    buttonSave.setOnAction( new EventHandler[ActionEvent] {
      //Create a map of varname and value and call the insetVarValues method with it.
      override def handle(event: ActionEvent): Unit = {

        var wim = new WritableImage(drawingWidth, drawingHeight)
        mainPane.getSnapShot(wim)


        var fileChooser = new FileChooser()
        fileChooser.setTitle("Save file")
        fileChooser.setInitialFileName("typeDrawing.png")
        var savedFile = fileChooser.showSaveDialog(stage)
        if (savedFile != null) {
          try
            ImageIO.write(SwingFXUtils.fromFXImage(wim, null), "png", savedFile)
          catch {
            case s: Exception =>

          }

          }



        }
    })


    var dimensionInputFields = ListBuffer[TextField]()
    //Create the Labels that display the types as String
    //Add Textfields that show the deflault grouping
    argTypes.foreach(argType => {
      var tf = new TextField()
      tf.setId(argTypes.indexOf(argType).toString)
      //new container per var
      var hBox = new HBox()
      dimensionInputFields+= tf
      hBox.getChildren.addAll(new Label("Dimension Grouping:"),tf)
      top.getChildren.addAll(
      new Label(argType.toString),hBox)})
    var dimensionInputFieldList = dimensionInputFields.toList

    //Combine ui elements
    //top.getChildren.addAll(typeLabels.toList)
    bottom.getChildren.addAll(mainPane,buttonSave)
    main.getChildren.addAll(top,middle, bottom)

    var scene = new javafx.scene.Scene(main)



    //Check if there are variables in the Type that need a value
    var varListBuffer =new ListBuffer[Var]
    argTypes.map(argType => varListBuffer.appendAll(argType.varList.toList))

    var arrayVars = varListBuffer.toList.distinct.sortWith(_.name>_.name)
    if (!arrayVars.isEmpty) {
      //If yes add an inputfield per variable
      var inputFieldBuffer = new ListBuffer[TextField]()

      arrayVars.foreach( arrayVar => {
        //new container per var
        var hBox = new HBox()
        //consists of label for varname and input field
        val varLabel = new Label(arrayVar.name)
        val textField = new TextField(defaultVarValue.toString)
        textField.setId(arrayVar.name)
        inputFieldBuffer += textField
        //add components together
        hBox.getChildren.addAll( varLabel, textField)
        middle.getChildren.add(hBox)
      })
        var inputFields = inputFieldBuffer.toList


      //Buttonlogic
      buttonDraw.setOnAction( new EventHandler[ActionEvent] {
        //Create a map of varname and value and call the insetVarValues method with it.
        override def handle(event: ActionEvent) {
          var allFilledOut = true
          inputFields.foreach( inputField => allFilledOut = !inputField.getText.trim.isEmpty)
          dimensionInputFieldList.foreach( inputField => allFilledOut = !inputField.getText.trim.isEmpty)

          if(allFilledOut){
            var userVarInput = new mutable.HashMap[String,Int]()
            var userDimInput = new mutable.HashMap[Int,List[List[Int]]]()
            inputFields.foreach( inputField=>
              try{
                userVarInput.put(inputField.getId, Integer.parseInt(inputField.getText()))
              }catch{
                case e: Exception =>
              } )

            dimensionInputFieldList.foreach( dimInputField => {
              try {
                userDimInput.put(dimensionInputFieldList.indexOf(dimInputField),parseDimensionGrouping(dimInputField.getText))
              } catch {
                case e: Exception =>
              }
            })
            try{
            //Finally draw the type
            var typesWithValues = insertVarValuesToAll(argTypes,userVarInput)

            draw(renderNodes(typesWithValues,userDimInput),mainPane)
            System.out.println(typesWithValues.toString)
          }catch{
            case te: ir.TypeException=> {
              var alert = new Alert(AlertType.INFORMATION)
              alert.setTitle("Information Dialog")
              alert.setHeaderText(null)
              alert.setContentText(te.msg)
              alert.showAndWait()
            }}
          }
        }
      })


      //Buttonlogic
      saveVars.setOnAction( new EventHandler[ActionEvent] {
        //Create a map of varname and value and call the insetVarValues method with it.
        override def handle(event: ActionEvent) {
          var allFilledOut = true
          inputFields.foreach( inputField => allFilledOut = !inputField.getText.trim.isEmpty)
          dimensionInputFieldList.foreach( inputField => allFilledOut = !inputField.getText.trim.isEmpty)

          if(allFilledOut){
            var userVarInput = new mutable.HashMap[String,Int]()
            inputFields.foreach( inputField=>
              try{
                userVarInput.put(inputField.getId, Integer.parseInt(inputField.getText()))
              }catch{
                case e: Exception =>
              } )

            try{
            //Finally draw the type
            var typesWithValues = insertVarValuesToAll(argTypes,userVarInput)
            var dimensionStringMap = new mutable.HashMap[Int,String]()
            typesWithValues.foreach(t => dimensionStringMap.put(typesWithValues.indexOf(t),getDimensionGrouping(t)))
            System.out.println(typesWithValues.toString)
            var index = 0;
            dimensionInputFieldList.foreach(t=>{
              t.setText(dimensionStringMap.get(index).get)
              index+=1
            })
            }catch{
              case te: ir.TypeException=> {
                var alert = new Alert(AlertType.INFORMATION)
                alert.setTitle("Information Dialog")
                alert.setHeaderText(null)
                alert.setContentText(te.msg)
                alert.showAndWait()
              }}
          }


        }
      })

      buttonDimension.setOnAction( new EventHandler[ActionEvent] {
        //Create a map of varname and value and call the insetVarValues method with it.
        override def handle(event: ActionEvent) {
          var allFilledOut = true
          inputFields.foreach( inputField => allFilledOut = !inputField.getText.trim.isEmpty)
          dimensionInputFieldList.foreach( inputField => allFilledOut = !inputField.getText.trim.isEmpty)

          if(allFilledOut){
            var userVarInput = new mutable.HashMap[String,Int]()
            var userDimInput = new mutable.HashMap[Int,List[List[Int]]]()
            inputFields.foreach( inputField=>
              try{
                userVarInput.put(inputField.getId, Integer.parseInt(inputField.getText()))
              }catch{
                case e: Exception =>
              } )

            dimensionInputFieldList.foreach( dimInputField => {
              try {
                userDimInput.put(dimensionInputFieldList.indexOf(dimInputField),parseDimensionGrouping(dimInputField.getText))
              } catch {
                case e: Exception =>
              }
            })
            try{
            //Finally draw the type
            var typesWithValues = insertVarValuesToAll(argTypes,userVarInput)
            draw(renderNodes(typesWithValues,userDimInput),mainPane)
            System.out.println(typesWithValues.toString)
            }catch{
              case te: ir.TypeException=> {
                var alert = new Alert(AlertType.INFORMATION)
                alert.setTitle("Information Dialog")
                alert.setHeaderText(null)
                alert.setContentText(te.msg)
                alert.showAndWait()
              }}
          }
        }
      })

        middle.getChildren.addAll(buttonDraw,saveVars,buttonDimension)
        middle.setVisible(true);
      try{
        //Try to draw with default values

        var typesWithValues = insertVarValuesToAll(argTypes,getDefaultValues(argTypes))
        var dimensionIndexMap = new mutable.HashMap[Int, List[List[Int]]]()
        var dimensionStringMap = new mutable.HashMap[Int,String]()
        typesWithValues.foreach(t => dimensionIndexMap.put(typesWithValues.indexOf(t),parseDimensionGrouping(getDimensionGrouping(t))))
        typesWithValues.foreach(t => dimensionStringMap.put(typesWithValues.indexOf(t),getDimensionGrouping(t)))

        var index = 0;
        dimensionInputFieldList.foreach(t=>{
          t.setText(dimensionStringMap.get(index).get)
          index+=1
        })
        draw(renderNodes(typesWithValues,dimensionIndexMap),mainPane)
        System.out.println(typesWithValues.toString)
      }catch{
        case te: ir.TypeException=> {
          var alert = new Alert(AlertType.INFORMATION)
          alert.setTitle("Information Dialog")
          alert.setHeaderText(null)
          alert.setContentText(te.msg)
          alert.showAndWait()
        }
        case e: Exception => {
          e.printStackTrace()
          System.out.println("Drawing with the default var value "+defaultVarValue+" failed.")
        }

      }
    }
    else {
      //If no vars need values to display then show it right away
      var dimensionIndexMap = new mutable.HashMap[Int, List[List[Int]]]()
      var dimensionStringMap = new mutable.HashMap[Int,String]()
      argTypes.foreach(t => dimensionIndexMap.put(argTypes.indexOf(t),parseDimensionGrouping(getDimensionGrouping(t))))
      argTypes.foreach(t => dimensionStringMap.put(argTypes.indexOf(t),getDimensionGrouping(t)))

      var index = 0;
      dimensionInputFieldList.foreach(t=>{
        t.setText(dimensionStringMap.get(index).get)
        index+=1
      })
      draw(renderNodes(argTypes,dimensionIndexMap),mainPane)
    }

    stage.setScene(scene)
    stage.setTitle("lift-paternoster")
    stage.show()
  }

  def getDimensionGrouping(argType:Type): String ={
    argType match {
      case ar: ArrayType with Size => "("+ar.size.evalInt+")"+ getDimensionGrouping(ar.elemT)
      case other => ""
    }
  }
  def parseDimensionGrouping(dimensionGrouping : String): List[List[Int]] ={
    var listBuffer = new ListBuffer[List[Int]]
    var groupingString = dimensionGrouping
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


def renderNodes(argTypes:List[Type],dimensionGrouping: mutable.HashMap[Int,List[List[Int]]]):Iterable[Graphics.GraphicalPrimitive]={
  var nodeBuffer = new ListBuffer[Iterable[Graphics.GraphicalPrimitive]]

  var firstType = true;
  var yMargin = 1f;
  var accHeight = 0d;

  argTypes.map(argType => nodeBuffer += utils.paternoster.logic.Scene.drawType(utils.paternoster.logic.Scene.typeNode(argType,dimensionGrouping.get(argTypes.indexOf(argType)).get)))

  var allAdjustedNodes = for(nodes <- nodeBuffer.toList) yield {

    var firstBox = nodes.maxBy(gp => gp match {
      case BoxWithText(text, bx, by, bwidth, bheight) => bwidth
      case Rectangle(x, y, w, h) => w
      case Box(x, y, w, h) => w
    })

    var adjustedNodes = firstBox match {
      case bwt: BoxWithText => Graphics.translateAll(nodes, -bwt.bx, -bwt.by)
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
  allAdjustedNodes.flatten
}




  def draw(prims: Iterable[Graphics.GraphicalPrimitive], drawPane: MainPane):Unit ={
    drawPane.draw(prims)
  }

  @throws(classOf[ir.TypeException])
  def insertVarValuesToAll(types: List[Type], values: mutable.HashMap[String,Int]):List[Type]={
    for( argType <- types) yield insertVarValues(argType,values)
  }

  /**
    * Replaces Var Types in the given Type t with matching values that are mapped to keys with the same name as the Var.
    * @param t The Type that will be modified
    * @param values A Map between varnames and values.
    * @return The new modified Type.
    */
  @throws(classOf[ir.TypeException])
  def insertVarValues(t : Type, values: mutable.HashMap[String,Int]): Type = {

      //Replace Var with Cst(value) if there is a matching value in "values".
      val arithExpVisiterFunc = (ae: ArithExpr)=>{
        ae match {
          case v: Var if(values.contains(v.name)) => ArithExpr.LongToCst(Int.int2long(values.get(v.name).get))
          case an:Any=>  an
        }
      }
      //Call the arithExprVisiterFunc on every ArithExpr inside the ArithExpr with visitAndRebuild
      val typeVisiterFunc = (ae:ArithExpr)=> ae.visitAndRebuild(arithExpVisiterFunc)

      //Call the function typeVisiterFunc at every ArithExpr of the type with visitAndRebuild
      var newT = Type.visitAndRebuild(t,typeVisiterFunc)
      newT



  }

  def getDefaultValues(types: List[Type]): mutable.HashMap[String,Int]={
    var defaultValues = new mutable.HashMap[String,Int]()
    //Check if there are variables in the Type that need a value
    var varListBuffer =new ListBuffer[Var]
    types.map(argType => varListBuffer.appendAll(argType.varList.toList))
    varListBuffer.toList.distinct.map(t => defaultValues.put(t.name,defaultVarValue))
    defaultValues
  }


}

object TypeVisualizer {
  var types: ListBuffer[Type] = null;
  var mainPane:MainPane = null;
  def apply(argType: Type,render:Boolean): Unit ={
    addType(argType)
    if(render) start()
  }
  def addType(argType: Type): Unit ={
    if(types == null) types= new ListBuffer[Type]()

    types+= argType
  }
  def getTypes():List[Type]={
    types.toList
  }
  def start(args:Array[String]= Array("default argument")) = {
    Application.launch(classOf[TypeVisualizer], args:_*)
  }
  def setMainPane(mainPane: MainPane): Unit ={
    this.mainPane = mainPane
  }
  def getMainPane():MainPane={
    this.mainPane
  }
}



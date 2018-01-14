package utils.paternoster.gui

import javafx.application.Application
import javafx.geometry.{Insets, Pos}
import javafx.stage.Stage
import javafx.scene.layout.VBox

import ir.{ArrayType, Size, Type}
import lift.arithmetic._
import javafx._
import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.layout.HBox
import javafx.scene.layout.Pane
import javafx.scene.paint.Color
import javafx.scene.text.Font

import utils.paternoster.logic.Graphics
import utils.paternoster.logic.Graphics.{Box, BoxWithText, GraphicalPrimitive, Rectangle}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TypeVisualizer() extends Application {


  override def start(stage: Stage): Unit = {
    //Get the Type
     var argType = TypeVisualizer.argType
    System.out.println(argType.toString)


    //mainPane is where we will draw
    val mainPane = new MainPane(1280, 720)



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
    val buttonSave = new Button()
    buttonSave.setText("Save")
    buttonSave.setPrefSize(100, 20)


    //Create the Label that displays the type as String
    var typeLabel = new Label()
    typeLabel.setText(argType.toString)

    //Combine ui elements
    top.getChildren.addAll(typeLabel)
    bottom.getChildren.addAll(mainPane)
    main.getChildren.addAll(top,middle, bottom)

    var scene = new javafx.scene.Scene(main, 1920, 1080)

    //Check if there are variables in the Type that need a value
    var arrayVars = argType.varList.distinct.sortWith(_.name>_.name)
    if (!arrayVars.isEmpty) {
      //If yes add an inputfield per variable
      var inputFieldBuffer = new ListBuffer[TextField]()

      arrayVars.foreach( arrayVar => {
        //new container per var
        var hBox = new HBox()
        //consists of label for varname and input field
        val varLabel = new Label(arrayVar.name)
        val textField = new TextField()
        textField.setId(arrayVar.name)
        inputFieldBuffer += textField
        //add components together
        hBox.getChildren.addAll( varLabel, textField)
        middle.getChildren.add(hBox)
      })

        var inputFields = inputFieldBuffer.toList


      //Buttonlogic
      buttonSave.setOnAction( new EventHandler[ActionEvent] {
        //Create a map of varname and value and call the insetVarValues method with it.
        override def handle(event: ActionEvent) {
          var allFilledOut = true
          inputFields.foreach( inputField => allFilledOut = !inputField.getText.trim.isEmpty)
          if(allFilledOut){
            var userInput = new mutable.HashMap[String,Int]()
            inputFields.foreach( inputField=>
              try{
                userInput.put(inputField.getId, Integer.parseInt(inputField.getText()))
              }catch{
                case e: Exception =>
              } )
            var newT = insertVarValues(argType,userInput)
            //Finally draw the type
            draw(renderNodes(newT),mainPane)
            System.out.println(newT.toString)
          }
        }
      })


        middle.getChildren.add(buttonSave)
        middle.setVisible(true);
    }
    else {
      //If no vars need values to display then show it right away
      draw(renderNodes(argType),mainPane)
    }

    stage.setScene(scene)
    stage.setTitle("lift-paternoster")
    stage.show()
  }


def renderNodes(argType:Type):Iterable[Graphics.GraphicalPrimitive]={
  var nodes = utils.paternoster.logic.Scene.drawType(utils.paternoster.logic.Scene.typeNode(argType))
  var firstBox =nodes.minBy(gp=> gp match {
    case BoxWithText(text,tx,ty,bx,by,bwidth,bheight)=> bx
    case Rectangle(x, y, w, h) =>x
    case Box(x, y, w, h) =>x
  })
  var adjustedNodes = firstBox match {
    case bwt:BoxWithText =>  Graphics.translateAll(nodes,-bwt.bx,-bwt.by)
  }
  adjustedNodes
}



  def draw(prims: Iterable[Graphics.GraphicalPrimitive], drawPane: MainPane):Unit ={
    drawPane.draw(prims)
  }

  /**
    * Replaces Var Types in the given Type t with matching values that are mapped to keys with the same name as the Var.
    * @param t The Type that will be modified
    * @param values A Map between varnames and values.
    * @return The new modified Type.
    */
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


}

object TypeVisualizer {
  var argType :Type = null;
  var mainPane:MainPane = null;
  def apply(argType: Type): Unit ={
    this.argType = argType
    start()
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



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

import utils.paternoster.logic.Graphics.Rectangle

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TypeVisualizer() extends Application {


  override def start(stage: Stage): Unit = {
    //Get the Type
     var argType = TypeVisualizer.argType
    System.out.println(argType.toString)

    //Start the App


    //Pane kann als Zeichenfläche genutzt werden//Pane kann als Zeichenfläche genutzt werden

    val mainPane = new MainPane(800, 600)

    //Vbox - Hauptlayout
    var main = new VBox();
    main.setSpacing(10);
    main.setPadding(new Insets(8, 8, 8, 8));



    //HBox top, obere Leiste mit Label
    val top: HBox = new HBox
    top.setSpacing(10);


    //HBox middle, obere Leiste mit Buttons
    val middle: HBox = new HBox
    middle.setSpacing(10);
    middle.setVisible(false);

    //Insert HBOX
    val bottom = new HBox
    bottom.setPadding(new Insets(0))
    bottom.setSpacing(8)
    bottom.setPrefHeight(800)
    bottom.setAlignment(Pos.CENTER)

    //Button to save var values
    val buttonSave = new Button()
    buttonSave.setText("Save")
    buttonSave.setPrefSize(100, 20)



    //Input field in top
    var typeLabel = new Label()
    typeLabel.setText(argType.toString)

    //Combine ui elements
    top.getChildren.addAll(typeLabel)
    bottom.getChildren.addAll(mainPane)
    main.getChildren.addAll(top,middle, bottom)

    var scene = new javafx.scene.Scene(main, 1000, 800)


    var arrayVars = argType.varList.distinct
    if (!arrayVars.isEmpty) {
      var inputFieldBuffer = new ListBuffer[TextField]()


      arrayVars.foreach( arrayVar => {
        //new row for each var
        var vBox = new VBox()
        //consists of label for varname and input field
        val varLabel = new Label(arrayVar.name)
        val textField = new TextField()
        textField.setId(arrayVar.name)

        inputFieldBuffer += textField

        vBox.getChildren.addAll( varLabel, textField)
        middle.getChildren.add(vBox)
      })

        var inputFields = inputFieldBuffer.toList


      //Buttonlogik
      buttonSave.setOnAction( new EventHandler[ActionEvent] {
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
            drawType(newT,mainPane)
            System.out.println(newT.toString)
          }
        }
      })


        middle.getChildren.add(buttonSave)
        middle.setVisible(true);
    }
    else {
      drawType(argType,mainPane)
    }

    stage.setScene(scene)
    stage.setTitle("lift-paternoster")
    stage.show()
    //val operation = LiftAST.Map(Function(Float, Array(Float,2)), 10)
    // val operation2 = LiftAST.Map(Function(Array(Float,2),Array(Array(Float,2), 2)),10)

    // mainPane.draw(utils.paternoster.logic.Scene.drawOperation(Scene.operationNode(operation2)))}
  }






  def drawType(argType : Type, drawPane: MainPane):Unit ={
    drawPane.draw(utils.paternoster.logic.Scene.drawType(utils.paternoster.logic.Scene.typeNode(argType)))
  }

  def insertVarValues(t : Type, values: mutable.HashMap[String,Int]): Type = {
    val arithExpVisiterFunc = (ae: ArithExpr)=>{
      ae match {
        case v: Var if(values.contains(v.name)) => ArithExpr.LongToCst(Int.int2long(values.get(v.name).get))
        case an:Any=>  an
      }
    }
    val typeVisiterFunc = (ae:ArithExpr)=> ae.visitAndRebuild(arithExpVisiterFunc)

    var newT = Type.visitAndRebuild(t,typeVisiterFunc)
    newT
  }


}

object TypeVisualizer {
  var argType :Type = null;
  def apply(argType: Type): Unit ={
    this.argType = argType
    start()
  }
  def start(args:Array[String]= Array("default argument")) = {
    Application.launch(classOf[TypeVisualizer], args:_*)
  }
}



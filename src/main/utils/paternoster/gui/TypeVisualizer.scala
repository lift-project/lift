package utils.paternoster.gui

import javafx.application.Application
import javafx.stage.Stage

import ir.Type


class TypeVisualizer() extends Application {
  override def start(stage: Stage): Unit = {
    //Get the Type
    val argType : Type = TypeVisualizer.argType
    System.out.println(argType.toString)
    //Start the App
    val mainPane = new MainPane(800, 600)
    stage.setScene(new javafx.scene.Scene(mainPane, mainPane.width, mainPane.height))
    stage.setTitle("lift-paternoster")
    stage.show()
    utils.paternoster.logic.Scene.drawType(utils.paternoster.logic.Scene.typeNode(argType))
    //val operation = LiftAST.Map(Function(Float, Array(Float,2)), 10)
   // val operation2 = LiftAST.Map(Function(Array(Float,2),Array(Array(Float,2), 2)),10)

   // mainPane.draw(utils.paternoster.logic.Scene.drawOperation(Scene.operationNode(operation2)))
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



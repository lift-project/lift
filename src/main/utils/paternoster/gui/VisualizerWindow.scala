package utils.paternoster.gui

import java.io.File
import javafx.application.Application
import javafx.embed.swing.SwingFXUtils
import javafx.event.{ActionEvent, EventHandler}
import javafx.geometry.{Insets, Pos}
import javafx.scene.{Node, Scene}
import javafx.scene.control._
import javafx.scene.control.Alert.AlertType
import javafx.scene.image.WritableImage
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.scene.layout.{HBox, VBox}
import javafx.stage.FileChooser.ExtensionFilter
import javafx.stage.{FileChooser, Stage}
import javax.imageio.ImageIO

import ir.{ArrayType, Size, Type}
import lift.arithmetic.{ArithExpr, Var}
import utils.paternoster.logic.Graphics
import utils.paternoster.logic.Graphics.{Box, BoxWithText, Rectangle}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class VisualizerWindow extends Application {


  val defaultVarValue = 4
  val DRAWING_WIDTH = 1280
  val DRAWING_HEIGHT = 720
  val DEFAULT_WINDOW_WIDTH = 1280
  val DEFAULT_WINDOW_HEIGHT = 720
  var visualizer: TypeVisualizer = null;

  override def start(stage: Stage): Unit = {
    //Get the Type
    var visualizer = TypeVisualizer.visualizer

    if(visualizer== null){
      System.exit(1)
    }

    //mainPane is where we will draw
    val mainPane = new MainPane(DRAWING_WIDTH, DRAWING_HEIGHT)

    TypeVisualizer.setMainPane(mainPane)

    //Vbox - Main layout container
    var main = new VBox();
    main.setSpacing(10);
    main.setPadding(new Insets(8, 8, 8, 8));

    val scrollPane = new ScrollPane()
    scrollPane.setContent(mainPane)
    scrollPane.setPrefViewportWidth(DEFAULT_WINDOW_WIDTH)
    scrollPane.setPrefViewportHeight(DEFAULT_WINDOW_HEIGHT)

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


    //Button to save image
    val buttonSave = new Button()
    buttonSave.setText("Save Image")
    buttonSave.setPrefSize(100, 20)
    //Buttonlogic
    buttonSave.setOnAction(new EventHandler[ActionEvent] {
      //Create a map of varname and value and call the insetVarValues method with it.
      override def handle(event: ActionEvent): Unit = {


        var fileChooser = new FileChooser()
        fileChooser.setTitle("Save file")
        fileChooser.setInitialFileName("typeDrawing")

        fileChooser.getExtensionFilters.add(new ExtensionFilter("Svg (*.svg)","*.svg"))
        fileChooser.getExtensionFilters.add(new ExtensionFilter("Png (*.png)","*.png"))

        var savedFile = fileChooser.showSaveDialog(stage)

        if (savedFile != null) {
          val extension = fileChooser.selectedExtensionFilterProperty.get.getExtensions.get(0).substring(1)
          if(!savedFile.getCanonicalPath.contains(".svg") && !savedFile.getCanonicalPath.contains(".png")){
            savedFile = new File(savedFile.getCanonicalPath+extension)
          }
          try
            visualizer.saveGraphic(savedFile, extension)
          catch {
            case nie: NotImplementedError => showAlert(nie.getMessage)
          }

        }


      }
    })


    var dimensionInputFields = ListBuffer[TextField]()
    val types = visualizer.getTypeVisualizations()
    //Create the Labels that display the types as String
    //Add Textfields that show the deflault grouping

    types.foreach(tv => {
      var tf = new TextField(visualizer.getDimensionGroupingAsString(tv.argType))
      tf.setId(tv.id.toString)
      //new container per var
      var hBox = new HBox()
      dimensionInputFields += tf
      hBox.getChildren.addAll(new Label("Dimension Grouping:"), tf)
      top.getChildren.addAll(
        new Label(visualizer.getCleanedTypeName(tv.argType)), hBox)
    })

    var dimensionInputFieldList = dimensionInputFields.toList

    //Combine ui elements
    //top.getChildren.addAll(typeLabels.toList)
    bottom.getChildren.addAll(scrollPane, buttonSave)
    main.getChildren.addAll(top, middle, bottom)

    var scene = new javafx.scene.Scene(main)


    //Check if there are variables in the Type that need a value
    var arrayVars = visualizer.getVariables()
    var inputFieldBuffer = new ListBuffer[TextField]()
    var inputFields: List[TextField] = null;

    if (!arrayVars.isEmpty) {
      //If yes add an inputfield per variable

      arrayVars.keySet.toList.sorted.foreach(arrayVar => {
        //new container per var
        var hBox = new HBox()
        //consists of label for varname and input field
        val varLabel = new Label(arrayVar)
        val textField = new TextField(arrayVars.get(arrayVar).get.toString)
        textField.setId(arrayVar)
        inputFieldBuffer += textField
        //add components together
        hBox.getChildren.addAll(varLabel, textField)
        middle.getChildren.add(hBox)
      })
      inputFields = inputFieldBuffer.toList
    }

    //Buttonlogic
    buttonDraw.setOnAction(new EventHandler[ActionEvent] {
      //Create a map of varname and value and call the insetVarValues method with it.
      override def handle(event: ActionEvent) {


        var success = updateModel(readVarInput(), readGroupingInput())
        if (success) {
          visualizer.draw(mainPane)
        }
      }
    })

    def readVarInput(): mutable.HashMap[String, String] ={
      var userVarInput: mutable.HashMap[String, String] = null;
      if (inputFields != null) {
        userVarInput = new mutable.HashMap[String, String]()
        //Read input fields
        inputFields.foreach(inputField => userVarInput.put(inputField.getId, inputField.getText()))
      }
      userVarInput
    }

    def readGroupingInput():mutable.HashMap[String, String]={
      var userDimInput: mutable.HashMap[String, String] = null;
      userDimInput = new mutable.HashMap[String, String]()
      //Read input fields
      dimensionInputFieldList.foreach(dimInputField => userDimInput.put(dimInputField.getId(), dimInputField.getText))
      userDimInput
    }

    scene.setOnKeyReleased(new EventHandler[KeyEvent]() {
      override def handle(event: KeyEvent): Unit = {
        var source =  event.getSource
        var node = source.asInstanceOf[Scene]
        System.out.println(node.toString())
        event.getCode match {
          case KeyCode.ENTER =>{

            var success = updateModel(readVarInput(), readGroupingInput())
            if (success) {
              visualizer.draw(mainPane)
            }
          }
          case default=>
        }
      }
    })



    middle.getChildren.addAll(buttonDraw)
    middle.setVisible(true);

    visualizer.draw(mainPane)

    stage.setScene(scene)
    stage.setTitle("lift-paternoster")
    stage.show()


    def updateModel(userVarInput: mutable.HashMap[String, String], userDimInput: mutable.HashMap[String, String]): Boolean = {
      var noExceptions = true
      if (userVarInput != null) {
        userVarInput.keySet.foreach(varName => {
          try {
            visualizer.updateVariable(varName, userVarInput.get(varName).get)
          } catch {
            case tex: ir.TypeException =>{
              showAlert(tex.msg)
              noExceptions = false
            }
            case nfe: NumberFormatException => {
              showAlert("Could not parse value for " + varName + ". Expected an an Integer but was \"" + userVarInput.get(varName).get + "\".")
              noExceptions = false
            }
          }
        })
      }
      if (userDimInput != null) {
        userDimInput.keySet.foreach(id => {
          try {
            visualizer.updateDimensionGrouping(id, userDimInput.get(id).get)
          } catch {
            case ill: IllegalArgumentException => {
              showAlert(ill.getMessage)
              noExceptions = false
              //Set textfields to default grouping
              dimensionInputFieldList.foreach(tf => tf.setText(visualizer.getDimensionGroupingAsString(visualizer.getTypeVisualizations().filter(tv => tv.id.toString.equals(tf.getId)).head.argType)))
            }
            case tex: ir.TypeException =>{
              showAlert(tex.msg)
              noExceptions = false
            }
            case ex: NumberFormatException => {
              System.out.println(ex.getClass.toString)
              showAlert("Could not parse dimension grouping for type #" + id + ". All brackets have to be either \"(Int)\" or \"(Int,Int)\".")
              noExceptions = false
            }
          }
        })
      }

      noExceptions
    }
  }
  def showAlert(msg:String):Unit ={
    var alert = new Alert(AlertType.ERROR)
    alert.setTitle("Error")
    alert.setHeaderText(null)
    alert.setResizable(true)
    alert.setContentText(msg)
    alert.showAndWait()
  }

}

package utils.paternoster.gui

import javafx.scene.canvas.Canvas
import javafx.scene.layout.Pane
import javafx.scene.paint.{Color, Paint}
import javafx.scene.text.Font

import utils.paternoster.logic.Graphics.GraphicalPrimitive


/**
  * Created by federico on 16/08/17.
  */

class MainPane(val width:Int, val height:Int) extends Pane {
  //General scaling
  var unitX = 120d
  var unitY = 60d
  //Used to separate things
  val smallX = 4
  val smallY = 4
  val canvas = new Canvas(width,height)
  //canvas.setScaleX(0.5)
  //canvas.setScaleY(0.5)
  this.getChildren.add(canvas)

  def draw(primitives:Iterable[GraphicalPrimitive]) = {
    val gc = this.canvas.getGraphicsContext2D
    gc.clearRect(0, 0, width, height)
    gc.setFill(Color.WHITE)
    gc.fillRect(0,0,width,height)
    gc.setFont(new Font(gc.getFont.getName,10))
    val context = JavaFXRenderer.Context(gc, unitX, unitY, smallX, smallY,width.toDouble,height.toDouble)
    JavaFXRenderer.drawPrimitives(primitives, context)
  }

}
